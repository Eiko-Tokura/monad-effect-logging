{-# LANGUAGE RecordWildCards #-}
-- | Some simple combinators to build your logger
module Module.Logging.Logger
  ( module Module.Logging.Logger
  , module System.Log.FastLogger
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Functor.Contravariant (contramap)
import Module.Logging
import System.Log.FastLogger
import qualified Control.Monad.Logger as ML

-- | It doesn't mean it is really fast, just because it is imported from fast-logger
createFastBaseLogger :: MonadIO m => LogType -> m (LogStr -> IO (), IO ())
createFastBaseLogger logT = liftIO $ newFastLogger logT

createStdoutBaseLogger :: MonadIO m => m (LogStr -> IO (), IO ())
createStdoutBaseLogger = createFastBaseLogger (LogStdout defaultBufSize)

createStderrBaseLogger :: MonadIO m => m (LogStr -> IO (), IO ())
createStderrBaseLogger = createFastBaseLogger (LogStderr defaultBufSize)

createFileLogger :: MonadIO m => FilePath -> m (LogStr -> IO (), IO ())
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
  let logLine = "[" <> foldl' (\x y -> x <> "|" <> y) "" (map someLogCatName types) <> "] " <> logStr
  logFunc $ Log types (logLine <> logStr)
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
