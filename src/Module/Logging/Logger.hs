{-# LANGUAGE RecordWildCards #-}
-- | Some simple combinators to build your logger
module Module.Logging.Logger
  ( module Module.Logging.Logger
  , module System.Log.FastLogger
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Effect
import Data.Time.Clock
import Module.Logging
import System.Log.FastLogger
import qualified Control.Monad.Logger as ML
import Data.List (foldl1')

data BaseLogger m = BaseLogger
  { baseLogFunc :: LogStr -> m ()
  , cleanUpFunc :: m ()
  }

liftBaseLogger :: (m () -> n ()) -> BaseLogger m -> BaseLogger n
liftBaseLogger nat (BaseLogger f c) = BaseLogger (nat . f) (nat c)
{-# INLINE liftBaseLogger #-}

instance Applicative m => Semigroup (BaseLogger m) where
  (BaseLogger f1 c1) <> (BaseLogger f2 c2) = BaseLogger (f1 *> f2) (c1 *> c2)
  {-# INLINE (<>) #-}
instance Applicative m => Monoid (BaseLogger m) where
  mempty = BaseLogger (const $ pure ()) (pure ())

-- | It doesn't mean it is really fast, just because it is imported from fast-logger
createFastBaseLogger :: MonadIO m => LogType -> m (BaseLogger IO)
createFastBaseLogger logT = liftIO $ uncurry BaseLogger <$> newFastLogger logT

createStdoutBaseLogger :: MonadIO m => m (BaseLogger IO)
createStdoutBaseLogger = createFastBaseLogger (LogStdout defaultBufSize)

createStderrBaseLogger :: MonadIO m => m (BaseLogger IO)
createStderrBaseLogger = createFastBaseLogger (LogStderr defaultBufSize)

createFileLogger :: MonadIO m => FilePath -> m (BaseLogger IO)
createFileLogger fp = createFastBaseLogger (LogFile (FileLogSpec fp (512 * 1024 * 1024) 3) defaultBufSize)

type Timed = Bool
-- | this formats the logging data and sends it to the provided function
simpleLogger :: Timed -> (LogStr -> IO ()) -> Logger IO LogData
simpleLogger time
  = contramap logSimple
  . typedLogger
  . (if time then timeLogger else id)
  . baseToLogger

-- | simply apply the provided function to the log string
baseToLogger :: (LogStr -> IO ()) -> Logger IO LogStr
baseToLogger baseIO = Logger $ \(Log _ str) -> baseIO str
{-# INLINE baseToLogger #-}

-- | add the types of the log to the log string on the left
typedLogger :: Logger IO LogStr -> Logger IO LogStr
typedLogger (Logger logFunc) = Logger $ \(Log types logStr) -> do
  let typeNames = map someLogCatName types
  let logLine
        | null typeNames = logStr
        | otherwise = "[" <> foldl1' (\x y -> x <> "|" <> y) typeNames <> "] " <> logStr
  logFunc $ Log types logLine
{-# INLINE typedLogger #-}

-- | add the current time to the log string on the left
timeLogger :: Logger IO LogStr -> Logger IO LogStr
timeLogger (Logger logger) = Logger $ \(Log types logStr) -> do
  time <- getCurrentTime
  let timeStr = toLogStr (show time)
  logger $ Log types (timeStr <> "|" <> logStr)
{-# INLINE timeLogger #-}

-- | format the log data into a simple log string
logSimple :: LogData -> LogStr
logSimple LogData {..}
    =  maybe "" ((<> ",") . toLogStr . ML.loc_filename) _logLoc
    <> maybe "" ((<> "-") . displayPos . ML.loc_start) _logLoc
    <> maybe "" ((<> "|") . displayPos . ML.loc_end) _logLoc
    <> maybe "" ((<> "|") . toLogStr) _logSource
    <> _logMsg
  where displayPos (l, c) = toLogStr (show l <> ":" <> show c)
{-# INLINE logSimple #-}

-- | A very simple function, make use of a TChan
makeConcurrentLogger :: MonadIO m => Logger IO LogData -> m (Logger IO LogData)
makeConcurrentLogger (Logger logger) = do
  queue <- liftIO newTChanIO
  _ <- liftIO $ forkIO $ forever $ do
    logItem <- atomically $ readTChan queue
    logger logItem
  return $ Logger $ \logItem -> liftIO $ atomically $ writeTChan queue logItem

-- $ Bracket pattern
-- | This function is used to create a logger in a scoped manner.
-- It takes care of creating and cleaning up the base logger.
--
-- Hint: use Ap and <> to combine multiple base loggers in IO (BaseLogger IO)
withBaseLogger
  :: (ConsFDataList c (LoggingModule : mods), MonadIO m, MonadMask m)
  => IO (BaseLogger IO)                       -- ^ specify a base logger
  -> ((LogStr -> IO ()) -> Logger IO LogData) -- ^ specify how to format the log data
  -> EffT' c (LoggingModule : mods) es m a
  -> EffT' c mods es m a
withBaseLogger createBaseLogger makeLogger action = bracketEffT
  (liftIO createBaseLogger)
  (\BaseLogger {cleanUpFunc} -> liftIO cleanUpFunc)
  (\BaseLogger {baseLogFunc} -> runLogging (makeLogger baseLogFunc) action)
