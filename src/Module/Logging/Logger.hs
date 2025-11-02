{-# LANGUAGE RecordWildCards #-}
-- | Some simple combinators to build your logger
module Module.Logging.Logger
  ( module Module.Logging.Logger
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

data BaseLogger m = BaseLogger
  { baseLogFunc :: LogStr -> m ()
  , cleanUpFunc :: m ()
  }

data LoggerWithCleanup m logS = LoggerWithCleanup (Logger m logS) (m ())

useBaseLogger :: ((LogStr -> m ()) -> Logger m logB) -> BaseLogger m -> LoggerWithCleanup m logB
useBaseLogger makeLogger (BaseLogger logFunc cleanUp) =
  LoggerWithCleanup (makeLogger logFunc) cleanUp
{-# INLINE useBaseLogger #-}

liftBaseLogger :: (m () -> n ()) -> BaseLogger m -> BaseLogger n
liftBaseLogger nat (BaseLogger f c) = BaseLogger (nat . f) (nat c)
{-# INLINE liftBaseLogger #-}

instance Applicative m => Semigroup (BaseLogger m) where
  (BaseLogger f1 c1) <> (BaseLogger f2 c2) = BaseLogger (\s -> f1 s *> f2 s) (c1 *> c2)
  {-# INLINE (<>) #-}
instance Applicative m => Monoid (BaseLogger m) where
  mempty = BaseLogger (const $ pure ()) (pure ())
  {-# INLINE mempty #-}

instance Applicative m => Semigroup (LoggerWithCleanup m logS) where
  (LoggerWithCleanup l1 c1) <> (LoggerWithCleanup l2 c2) = LoggerWithCleanup (l1 <> l2) (c1 *> c2)
  {-# INLINE (<>) #-}
instance Applicative m => Monoid (LoggerWithCleanup m logS) where
  mempty = LoggerWithCleanup mempty (pure ())
  {-# INLINE mempty #-}

-- | It doesn't mean it is really fast, just because it is imported from fast-logger
createFastBaseLogger :: MonadIO m => LogType -> m (BaseLogger IO)
createFastBaseLogger logT = liftIO $ uncurry BaseLogger <$> newFastLogger logT

-- | Uses the logger from fast-logger with default buffer-size
createStdoutBaseLogger :: MonadIO m => m (BaseLogger IO)
createStdoutBaseLogger = createFastBaseLogger (LogStdout defaultBufSize)

-- | A very simple logger that just prints to stdout **without buffering**.
-- suitable for simple and fast-reaction applications
createSimpleStdoutBaseLogger :: MonadIO m => m (BaseLogger IO)
createSimpleStdoutBaseLogger = liftIO $ do
  let logFunc (LogStr _ builder) = BL.putStr (BB.toLazyByteString builder)
  return $ BaseLogger logFunc (return ())
{-# INLINE createSimpleStdoutBaseLogger #-}

-- | A very simple concurrent logger that just prints to stdout **without buffering**.
-- A cleanUp function is provided to make sure all logs are printed before exiting.
createSimpleConcurrentStdoutBaseLogger :: MonadIO m => m (BaseLogger IO)
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
  return $ BaseLogger logFunc cleanUpFunc

-- | Uses the logger from fast-logger with default buffer-size
createStderrBaseLogger :: MonadIO m => m (BaseLogger IO)
createStderrBaseLogger = createFastBaseLogger (LogStderr defaultBufSize)

-- | Uses the logger from fast-logger with default buffer-size
createFileLogger :: MonadIO m => FilePath -> m (BaseLogger IO)
createFileLogger fp = createFastBaseLogger (LogFile (FileLogSpec fp (512 * 1024 * 1024) 3) defaultBufSize)

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
--
-- Example:
-- @
-- logWithRendering someRenderFunc (simpleLogger False baseLogFunc) :: Logger m (LogMsg b)
-- @
logWithRendering :: (b -> LogStr) -> Logger m LogS -> Logger m (LogMsg b)
logWithRendering renderB (Logger logFunc) = Logger $ \logB -> do
  let logS = fmap renderB <$> logB
  logFunc logS
{-# INLINE logWithRendering #-}

-- | simply apply the provided function to the log string
baseToLogger :: (LogStr -> m ()) -> Logger m LogStr
baseToLogger baseIO = Logger $ \(Log _ str) -> baseIO str
{-# INLINE baseToLogger #-}

-- | add the types of the log to the log string on the left
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

-- | format the log data into a simple log string
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
withLogger
  :: (ConsFDataList c (Logging m log : mods), Monad m, MonadMask m)
  => LoggerWithCleanup m log -- ^ specify a logger with cleanup function
  -> EffT' c (Logging m log : mods) es m a
  -> EffT' c mods es m a
withLogger (LoggerWithCleanup logger cleanUp) action = bracketEffT
  (return ())
  (\_ -> lift cleanUp)
  (\_ -> runLogging logger action)
{-# INLINE withLogger #-}

-- $ Bracket pattern
-- | This function is used to create a logger in a scoped manner.
-- It takes care of creating and cleaning up the base logger.
--
-- Hint: use Ap and <> to combine multiple base loggers in IO (BaseLogger IO)
withBaseLogger
  :: (ConsFDataList c (Logging m (LogMsg logS) : mods), Monad m, MonadMask m)
  => m (BaseLogger m)                       -- ^ specify a base logger
  -> ((LogStr -> m ()) -> Logger m (LogMsg logS)) -- ^ specify how to format the log data using the base logger
  -> EffT' c (Logging m (LogMsg logS) : mods) es m a
  -> EffT' c mods es m a
withBaseLogger createBaseLogger makeLogger action = bracketEffT
  (lift createBaseLogger)
  (\BaseLogger {cleanUpFunc} -> lift cleanUpFunc)
  (\BaseLogger {baseLogFunc} -> runLogging (makeLogger baseLogFunc) action)
{-# INLINE withBaseLogger #-}

withBaseLoggerIO
  :: IO (BaseLogger IO)                    -- ^ specify a base logger
  -> ((LogStr -> IO ()) -> Logger IO logS) -- ^ specify how to log logS using the base logger
  -> (Logger IO logS -> IO a)  -- ^ action to run with the logger
  -> IO a
withBaseLoggerIO createBaseLogger makeLogger action = bracket
  (liftIO createBaseLogger)
  (\BaseLogger {cleanUpFunc} -> liftIO cleanUpFunc)
  (\BaseLogger {baseLogFunc} -> action (makeLogger baseLogFunc))
{-# INLINE withBaseLoggerIO #-}
