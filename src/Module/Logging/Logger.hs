{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}
-- | Some simple combinators to build your logger
module Module.Logging.Logger
  ( module Module.Logging.Logger
  -- * Re-exporting fast-logger
  , module System.Log.FastLogger
  ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Effect
import Data.Time.Clock
import Module.Logging
import System.Log.FastLogger
import System.Log.FastLogger.Internal (LogStr (..))
import qualified Control.Monad.Logger as ML
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl1')
import Control.Exception (bracket)

-- | Logger with a cleanup function
data LoggerWithCleanup m log = LoggerWithCleanup
  { baseLogFunc :: log -> m ()
  , cleanUpFunc :: m ()
  }

useBaseLogger :: ((LogStr -> m ()) -> Logger m logS) -> LoggerWithCleanup m LogStr -> LoggerWithCleanup m (Log logS)
useBaseLogger makeLogger (LoggerWithCleanup logFunc cleanUp) =
  LoggerWithCleanup (_runLogger $ makeLogger logFunc) cleanUp
{-# INLINE useBaseLogger #-}

-- | Primitive version of 'useBaseLogger'
useBaseLogger' :: Monad m => (Log logS -> m LogStr) -> LoggerWithCleanup m LogStr -> LoggerWithCleanup m (Log logS)
useBaseLogger' makeLogger (LoggerWithCleanup logFunc cleanUp) =
  LoggerWithCleanup (makeLogger >=> logFunc) cleanUp
{-# INLINE useBaseLogger' #-}

liftBaseLogger :: (m () -> n ()) -> LoggerWithCleanup m log -> LoggerWithCleanup n log
liftBaseLogger nat (LoggerWithCleanup f c) = LoggerWithCleanup (nat . f) (nat c)
{-# INLINE liftBaseLogger #-}

instance Applicative m => Semigroup (LoggerWithCleanup m log) where
  (LoggerWithCleanup l1 c1) <> (LoggerWithCleanup l2 c2) = LoggerWithCleanup (\l -> l1 l *> l2 l) (c1 *> c2)
  {-# INLINE (<>) #-}
instance Applicative m => Monoid (LoggerWithCleanup m log) where
  mempty = LoggerWithCleanup (const $ pure ()) (pure ())
  {-# INLINE mempty #-}

-- | It doesn't mean it is really fast, called 'FastBaseLogger' because it is imported from fast-logger
createFastBaseLogger :: MonadIO m => LogType -> m (LoggerWithCleanup IO LogStr)
createFastBaseLogger logT = liftIO $ uncurry LoggerWithCleanup <$> newFastLogger logT

-- | Uses the logger from fast-logger with default buffer-size
createStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createStdoutBaseLogger = createFastBaseLogger (LogStdout defaultBufSize)

-- | A very simple logger that just prints to stdout **without buffering**.
-- Inlines a putStr function. Suitable for simple applications
createSimpleStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createSimpleStdoutBaseLogger = liftIO $ do
  let logFunc (LogStr _ builder) = BL.putStr (BB.toLazyByteString builder)
  return $ LoggerWithCleanup logFunc (return ())
{-# INLINE createSimpleStdoutBaseLogger #-}

-- | A very simple concurrent logger that just prints to stdout **without buffering**.
-- A cleanUp function is provided to make sure all logs are printed before exiting.
createSimpleConcurrentStdoutBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createSimpleConcurrentStdoutBaseLogger = liftIO $ do
  queue <- newTQueueIO
  counter <- newTVarIO (0 :: Int)
  -- ^ the caller increments this when logging atomically
  -- logger checks this to see if it should exit
  let logFunc (LogStr _ builder) = do
        atomically $ do
          writeTQueue queue builder
          modifyTVar' counter (+1)
      rawLogFunc builder = BL.putStr (BB.toLazyByteString builder)
      atomicLogFunc queue' = do
        logStr <- atomically $ do
          logStr <- readTQueue queue'
          modifyTVar' counter (subtract 1)
          return logStr
        rawLogFunc logStr
  let cleanUpFunc = do
        remQ <- atomically $ do
          r <- readTVar counter
          if r == 0
            then return Nothing
            else do
              b <- flushTQueue queue
              writeTVar counter 0
              return (Just b)
        forM_ remQ (mapM_ rawLogFunc)
  _ <- forkIO $ forever $ atomicLogFunc queue
  return $ LoggerWithCleanup logFunc cleanUpFunc

-- | Uses the logger from fast-logger with default buffer-size
createStderrBaseLogger :: MonadIO m => m (LoggerWithCleanup IO LogStr)
createStderrBaseLogger = createFastBaseLogger (LogStderr defaultBufSize)

-- | Uses the logger from fast-logger with default buffer-size
createFileLogger :: MonadIO m => FilePath -> m (LoggerWithCleanup IO LogStr)
createFileLogger fp = createFastBaseLogger (LogFile (FileLogSpec fp (256 * 1024 * 1024) 2) defaultBufSize)

-- | Uses the logger from fast-logger with custom size and number of rotated files
createFileLoggerWith :: MonadIO m => Integer -> Int -> FilePath -> m (LoggerWithCleanup IO LogStr)
createFileLoggerWith size n fp = createFastBaseLogger (LogFile (FileLogSpec fp size n) defaultBufSize)

type Timed = Bool
-- | this formats the logging data and sends it to the provided function
simpleLogger :: MonadIO m => Timed -> (LogStr -> m ()) -> Logger m LogS
simpleLogger time
  = contramap logSimple
  . contramap (<> "\n")
  . (if time then timeLogger else id)
  . typedLogger
  . baseToLogger

-- | Render the inner data type into LogStr and pass to the provided logger accepting LogStr.
-- @
-- logWithRendering renderB = contramap $ fmap renderB
-- @
--
-- Example:
-- @
-- logWithRendering someRenderFunc (simpleLogger False baseLogFunc) :: Logger m (LogMsg b)
-- @
logWithRendering :: (b -> LogStr) -> Logger m LogS -> Logger m (LogMsg b)
logWithRendering renderB = contramap $ fmap renderB
{-# INLINE logWithRendering #-}

-- | simply apply the provided function to the log string
baseToLogger :: (LogStr -> m ()) -> Logger m LogStr
baseToLogger baseIO = Logger $ \(Log _ str) -> baseIO str
{-# INLINE baseToLogger #-}

-- | Modifies LogStr: add the types of the log to the log string on the left
typedLogger :: Logger m LogStr -> Logger m LogStr
typedLogger (Logger logFunc) = Logger $ \(Log types logStr) -> do
  let typeNames = map someLogCatName types
  let logLine
        | null typeNames = logStr
        | otherwise = "[" <> foldl1' (\x y -> x <> "|" <> y) typeNames <> "] " <> logStr
  logFunc $ Log types logLine
{-# INLINE typedLogger #-}

-- | add the current time to the log string on the left
timeLogger :: MonadIO m => Logger m LogStr -> Logger m LogStr
timeLogger (Logger logger) = Logger $ \(Log types logStr) -> do
  time <- liftIO getCurrentTime
  let timeStr = toLogStr (show time)
  logger $ Log types (timeStr <> "|" <> logStr)
{-# INLINE timeLogger #-}

-- | format the log data into a simple log string, utilizing log types, and location info (if any)
logSimple :: LogS -> LogStr
logSimple LogMsg {..}
    =  maybe "" ((<> ",") . toLogStr . ML.loc_filename) _logLoc
    <> maybe "" ((<> "-") . displayPos . ML.loc_start) _logLoc
    <> maybe "" ((<> "|") . displayPos . ML.loc_end) _logLoc
    <> maybe "" ((<> "|") . toLogStr) _logSource
    <> _logMsg
  where displayPos (l, c) = toLogStr (show l <> ":" <> show c)
{-# INLINE logSimple #-}

-- | Bracket pattern, runs the action with the provided logger and cleans up afterwards
withLoggerCleanup
  :: (ConsFDataList c (Logging m logS : mods), Monad m, MonadMask m)
  => LoggerWithCleanup m (Log logS) -- ^ specify a logger with cleanup function
  -> EffT' c (Logging m logS : mods) es m a
  -> EffT' c mods es m a
withLoggerCleanup (LoggerWithCleanup logger cleanUp) action = bracketEffT
  (return ())
  (\_ -> lift cleanUp)
  (\_ -> runLogging (Logger logger) action)
{-# INLINE withLoggerCleanup #-}

-- $ Bracket pattern
-- | This function is used to create a logger in a scoped manner.
-- It takes care of creating and cleaning up the base logger.
withBaseLogger
  :: (ConsFDataList c (Logging m (LogMsg logS) : mods), Monad m, MonadMask m)
  => m (LoggerWithCleanup m LogStr)               -- ^ specify a base logger
  -> ((LogStr -> m ()) -> Logger m (LogMsg logS)) -- ^ specify how to format the log data using the base logger
  -> EffT' c (Logging m (LogMsg logS) : mods) es m a
  -> EffT' c mods es m a
withBaseLogger createBaseLogger makeLogger action = bracketEffT
  (lift createBaseLogger)
  (\LoggerWithCleanup {cleanUpFunc} -> lift cleanUpFunc)
  (\LoggerWithCleanup {baseLogFunc} -> runLogging (makeLogger baseLogFunc) action)
{-# INLINE withBaseLogger #-}

withBaseLoggerIO
  :: IO (LoggerWithCleanup IO LogStr)                    -- ^ specify a base logger
  -> ((LogStr -> IO ()) -> Logger IO logS) -- ^ specify how to log logS using the base logger
  -> (Logger IO logS -> IO a)  -- ^ action to run with the logger
  -> IO a
withBaseLoggerIO createBaseLogger makeLogger action = bracket
  (liftIO createBaseLogger)
  (\LoggerWithCleanup {cleanUpFunc} -> liftIO cleanUpFunc)
  (\LoggerWithCleanup {baseLogFunc} -> action (makeLogger baseLogFunc))
{-# INLINE withBaseLoggerIO #-}
