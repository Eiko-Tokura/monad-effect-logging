{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Module.Logging.Logger
  ( -- * Logger Lifecycle
    LoggerWithCleanup(..)
  , liftBaseLogger
    -- * Logger Options
  , LoggerOptions(..)
  , LogOrderControl(..)
  , LoggerStyle
  , defaultLoggerStyle
  , buildLoggerStyle
  , loggerUseAnsi
  , loggerNoStyle
  , loggerShowWith
  , loggerNoTime
  , loggerNoCats
  , loggerNoLoc
  , loggerNoSource
  , loggerNoNewline
  , loggerOrder
    -- * Base Loggers
  , createFastBaseLogger
  , createStdoutBaseLogger
  , createSimpleStdoutBaseLogger
  , createSimpleConcurrentStdoutBaseLogger
  , createStderrBaseLogger
  , createFileLogger
  , createFileLoggerWith
    -- * Rendering and Composition
  , renderLogEvent
  , loggerFromRenderer
  , makeLoggerFromBase
  , withLoggerCleanup
  , withBaseLogger
  , withBaseLoggerIO
    -- * Helpers
  , defaultLoggingFromEnv
  , defaultLoggingFromArgs
  , defaultLoggingOptParser
    -- * Re-exporting fast-logger
  , module System.Log.FastLogger
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Effect
import Control.System (detectFlag, detectAllFlags)
import Data.Text (Text)
import Data.Time.Clock
import Module.Logging
import System.Environment (lookupEnv)
import System.Log.FastLogger
import System.Log.FastLogger.Internal (LogStr(..))
import Text.Read (readMaybe)
import qualified Control.Monad.Logger as ML
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Options.Applicative as O
import Data.Maybe

data LoggerWithCleanup m a = LoggerWithCleanup
  { baseLogFunc :: a -> m ()
  , cleanUpFunc :: m ()
  }

data LogOrderControl
  = LogTimeChunk
  | LogCatChunk
  | LogLocChunk
  | LogSrcChunk
  | LogDocChunk
  deriving (Eq, Ord, Show)

data LoggerOptions = LoggerOptions
  { loggerDocRenderOptions :: DocRenderOptions
  , loggerIncludeTime      :: Bool
  , loggerIncludeCats      :: Bool
  , loggerIncludeLoc       :: Bool
  , loggerIncludeSource    :: Bool
  , loggerAppendNewline    :: Bool
  , loggerOrderControl     :: Maybe [LogOrderControl]
  }

type LoggerStyle = LoggerOptions -> LoggerOptions

defaultLoggerStyle :: LoggerOptions
defaultLoggerStyle =
  LoggerOptions
    { loggerDocRenderOptions = defaultDocRenderOptions
    , loggerIncludeTime      = True
    , loggerIncludeCats      = True
    , loggerIncludeLoc       = True
    , loggerIncludeSource    = True
    , loggerAppendNewline    = True
    , loggerOrderControl     = Nothing
    }

buildLoggerStyle :: LoggerStyle -> LoggerOptions
buildLoggerStyle style = style defaultLoggerStyle

loggerUseAnsi :: LoggerStyle
loggerUseAnsi opts =
  opts
    { loggerDocRenderOptions =
        (loggerDocRenderOptions opts) { docRenderStyleMode = AnsiStyles }
    }

loggerNoStyle :: LoggerStyle
loggerNoStyle opts =
  opts
    { loggerDocRenderOptions =
        (loggerDocRenderOptions opts) { docRenderStyleMode = NoStyles }
    }

loggerShowWith :: (forall a. Show a => a -> ML.LogStr) -> LoggerStyle
loggerShowWith renderShown opts =
  opts
    { loggerDocRenderOptions =
        (loggerDocRenderOptions opts) { docRenderShow = renderShown }
    }

loggerNoTime :: LoggerStyle
loggerNoTime opts = opts {loggerIncludeTime = False}

loggerNoCats :: LoggerStyle
loggerNoCats opts = opts {loggerIncludeCats = False}

loggerNoLoc :: LoggerStyle
loggerNoLoc opts = opts {loggerIncludeLoc = False}

loggerNoSource :: LoggerStyle
loggerNoSource opts = opts {loggerIncludeSource = False}

loggerNoNewline :: LoggerStyle
loggerNoNewline opts = opts {loggerAppendNewline = False}

loggerOrder :: [LogOrderControl] -> LoggerStyle
loggerOrder chunks opts = opts {loggerOrderControl = Just chunks}

liftBaseLogger :: (m () -> n ()) -> LoggerWithCleanup m a -> LoggerWithCleanup n a
liftBaseLogger nat (LoggerWithCleanup f cleanup) =
  LoggerWithCleanup (nat . f) (nat cleanup)

instance Applicative m => Semigroup (LoggerWithCleanup m a) where
  LoggerWithCleanup logA cleanA <> LoggerWithCleanup logB cleanB =
    LoggerWithCleanup (\entry -> logA entry *> logB entry) (cleanA *> cleanB)

instance Applicative m => Monoid (LoggerWithCleanup m a) where
  mempty = LoggerWithCleanup (const $ pure ()) (pure ())

createFastBaseLogger :: MonadIO m => LogType -> m (LoggerWithCleanup IO LogStr)
createFastBaseLogger logType =
  liftIO $ uncurry LoggerWithCleanup <$> newFastLogger logType

createStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createStdoutBaseLogger = createFastBaseLogger (LogStdout defaultBufSize)

createSimpleStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createSimpleStdoutBaseLogger =
  liftIO $ do
    let logFunc (LogStr _ builder) = BL.putStr (BB.toLazyByteString builder)
    pure $ LoggerWithCleanup logFunc (pure ())

createSimpleConcurrentStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createSimpleConcurrentStdoutBaseLogger =
  liftIO $ do
    queue <- newTQueueIO
    counter <- newTVarIO (0 :: Int)
    let logFunc (LogStr _ builder) =
          atomically $ do
            writeTQueue queue builder
            modifyTVar' counter (+ 1)
        rawLogFunc builder = BL.putStr (BB.toLazyByteString builder)
        atomicLogFunc queue' = do
          builder <- atomically $ do
            next <- readTQueue queue'
            modifyTVar' counter (subtract 1)
            pure next
          rawLogFunc builder
        cleanUpFunc tid = do
          killThread tid
          remaining <- atomically $ do
            count <- readTVar counter
            if count == 0
              then pure Nothing
              else do
                builders <- flushTQueue queue
                writeTVar counter 0
                pure (Just builders)
          forM_ remaining (mapM_ rawLogFunc)
    tid <- forkIO $ forever $ atomicLogFunc queue
    pure $ LoggerWithCleanup logFunc (cleanUpFunc tid)

createStderrBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createStderrBaseLogger = createFastBaseLogger (LogStderr defaultBufSize)

createFileLogger :: MonadIO m => FilePath -> m (LoggerWithCleanup IO LogStr)
createFileLogger fp =
  createFastBaseLogger (LogFile (FileLogSpec fp (256 * 1024 * 1024) 2) defaultBufSize)

createFileLoggerWith :: MonadIO m => Integer -> Int -> FilePath -> m (LoggerWithCleanup IO LogStr)
createFileLoggerWith size count fp =
  createFastBaseLogger (LogFile (FileLogSpec fp size count) defaultBufSize)

renderLogEvent :: LoggerOptions -> LogEvent (LogWithSourceMeta LogDoc) -> IO ML.LogStr
renderLogEvent LoggerOptions {..} entry = do
  let meta = entry ^. logEventPayload
      docChunk = renderLogDoc loggerDocRenderOptions (meta ^. logMetaDoc)
      catChunk =
        if loggerIncludeCats && not (null (entry ^. logEventCats))
          then "[" <> mconcat (intersperse "|" (map (renderLogDoc loggerDocRenderOptions . someLogCatDisplay) (entry ^. logEventCats))) <> "] "
          else mempty
      locChunk =
        if loggerIncludeLoc
          then maybe mempty renderLoc (meta ^. logMetaLoc)
          else mempty
      srcChunk =
        if loggerIncludeSource
          then maybe mempty ((<> "|") . ML.toLogStr) (meta ^. logMetaSource)
          else mempty
      suffix = if loggerAppendNewline then "\n" else mempty
      orderControl = fromMaybe [LogTimeChunk, LogCatChunk, LogLocChunk, LogSrcChunk, LogDocChunk] loggerOrderControl
  timeChunk <-
    if loggerIncludeTime
      then do
        now <- getCurrentTime
        pure $ ML.toLogStr (show now) <> "|"
      else pure mempty
  pure $ mconcat (map (\case
           LogTimeChunk -> timeChunk
           LogCatChunk  -> catChunk
           LogLocChunk  -> locChunk
           LogSrcChunk  -> srcChunk
           LogDocChunk  -> docChunk
         ) orderControl) <> suffix
  where
    intersperse _ [] = []
    intersperse sep (x:xs) = x : prependAll xs
      where
        prependAll [] = []
        prependAll (y:ys) = sep : y : prependAll ys

    renderLoc loc =
      ML.toLogStr (ML.loc_filename loc)
        <> "-"
        <> displayPos (ML.loc_start loc)
        <> "|"
        <> displayPos (ML.loc_end loc)
        <> "|"

    displayPos (line, col) = ML.toLogStr (show line <> ":" <> show col)

loggerFromRenderer
  :: MonadIO m
  => LoggerOptions
  -> (ML.LogStr -> m ())
  -> Logger m (LogWithSourceMeta LogDoc)
loggerFromRenderer opts sink =
  Logger $ \entry -> do
    rendered <- liftIO $ renderLogEvent opts entry
    sink rendered

makeLoggerFromBase
  :: MonadIO m
  => LoggerOptions
  -> LoggerWithCleanup m ML.LogStr
  -> LoggerWithCleanup m (LogEvent (LogWithSourceMeta LogDoc))
makeLoggerFromBase opts LoggerWithCleanup {..} =
  LoggerWithCleanup
    { baseLogFunc = runLogger (loggerFromRenderer opts baseLogFunc)
    , cleanUpFunc = cleanUpFunc
    }

withLoggerCleanup
  :: (ConsFDataList c (LogEffect m a : mods), Monad m, MonadMask m)
  => LoggerWithCleanup m (LogEvent (LogWithSourceMeta a))
  -> EffT' c (LogEffect m a : mods) es m b
  -> EffT' c mods es m b
withLoggerCleanup (LoggerWithCleanup logger cleanup) action =
  bracketEffT
    (pure ())
    (\_ -> lift cleanup)
    (\_ -> runLogEffect (Logger logger) action)

withBaseLogger
  :: (ConsFDataList c (LogEffect m LogDoc : mods), MonadIO m, MonadMask m)
  => m (LoggerWithCleanup m ML.LogStr)
  -> LoggerOptions
  -> EffT' c (LogEffect m LogDoc : mods) es m a
  -> EffT' c mods es m a
withBaseLogger createBaseLogger opts action =
  bracketEffT
    (lift createBaseLogger)
    (\LoggerWithCleanup {cleanUpFunc} -> lift cleanUpFunc)
    (\baseLogger ->
       let logger = Logger (baseLogFunc (makeLoggerFromBase opts baseLogger))
        in runLogEffect logger action
    )

withBaseLoggerIO
  :: IO (LoggerWithCleanup IO ML.LogStr)
  -> LoggerOptions
  -> (Logger IO (LogWithSourceMeta LogDoc) -> IO a)
  -> IO a
withBaseLoggerIO createBaseLogger opts action =
  bracket
    createBaseLogger
    cleanUpFunc
    (\baseLogger -> action $ loggerFromRenderer opts baseLogger.baseLogFunc)

----

defaultLoggingFromEnv
  :: LoggerWithCleanup IO (LogEvent (LogWithSourceMeta LogDoc))
  -> IO (ModuleInitData LoggingModule)
defaultLoggingFromEnv (LoggerWithCleanup logger cleanup) = do
  mLevel <- (readMaybe =<<) <$> lookupEnv "LOG_LEVEL"
  pure $ LogEffectInitData (Logger logger) (Just cleanup) id mLevel

defaultLoggingFromArgs
  :: LoggerWithCleanup IO (LogEvent (LogWithSourceMeta LogDoc))
  -> [String]
  -> Either Text (ModuleInitData LoggingModule)
defaultLoggingFromArgs (LoggerWithCleanup logger cleanup) [] =
  Right $ LogEffectInitData (Logger logger) (Just cleanup) id Nothing
defaultLoggingFromArgs (LoggerWithCleanup logger cleanup) args = do
  level <- maybe (Right Nothing) (fmap Just) $ detectFlag "--log-level" defaultStringToLogSeverity args
  types <- sequence $ detectAllFlags "--log-type" (\case "" -> Left "Empty log type"; s -> Right s) args
  nonTypes <- sequence $ detectAllFlags "--no-log-type" (\case "" -> Left "Empty log type"; s -> Right s) args
  let transform =
        foldr
          (.)
          id
          ( [anyLogCat (isLogCatName name) | name <- types]
              <> [excludeLogCat (isLogCatName name) | name <- nonTypes]
          )
  pure $ LogEffectInitData (Logger logger) (Just cleanup) transform level

defaultLoggingOptParser
  :: Applicative m
  => LoggerWithCleanup m (LogEvent (LogWithSourceMeta LogDoc))
  -> O.Parser (ModuleInitData (LogEffect m LogDoc))
defaultLoggingOptParser (LoggerWithCleanup logger cleanup) = do
  level <-
    O.optional $
      O.option O.auto
        ( O.long "log-level"
            <> O.metavar "LEVEL"
            <> O.help "Log level, one of 'Debug', 'Info', 'Warn', 'Error', or a number between 0 and 10 with a precision of 1 decimal place"
        )
  types :: [String] <-
    O.many $
      O.option O.str
        ( O.long "log-type"
            <> O.metavar "TYPE"
            <> O.help "Log type, can be specified multiple times"
        )
  nonTypes :: [String] <-
    O.many $
      O.option O.str
        ( O.long "no-log-type"
            <> O.metavar "TYPE"
            <> O.help "Log type to exclude, can be specified multiple times"
        )
  pure $
    LogEffectInitData
      (Logger logger)
      (Just cleanup)
      (foldr (.) id $ [anyLogCat (isLogCatName name) | name <- types] <> [excludeLogCat (isLogCatName name) | name <- nonTypes])
      level
