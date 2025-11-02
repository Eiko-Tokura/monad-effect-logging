{-# LANGUAGE QuasiQuotes, DeriveLift #-}
-- | This module provides functionality for handling trace IDs in logging.
--
--  A trace Id is a unique identifier used to trace and correlate log entries across different parts of a system.
--  It is particularly useful in systems for tracking requests as they propagate through various services.
module Module.Logging.TraceId where

import Control.Concurrent.STM
import Control.Monad.Effect
import Control.Monad.Logger
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.TypeList
import Data.Word
import Data.Aeson (FromJSON, ToJSON)
import Module.Logging
import Module.RS.QQ
import Module.Logging.TraceId.XorShiftRNG

newtype TraceId = TraceId { unTraceId :: Word64 }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance IsLogCat TraceId where
  logTypeDisplay (TraceId tid) = "TID=" <> toLogStr tid
  {-# INLINE logTypeDisplay #-}

[makeRModule__|
TraceIdGen
  newTraceId :: !(IO TraceId)
|]

[makeRModule__|
WithTraceId
  traceId :: !TraceId
|]

-- | Assign the provided traceId to the logging context
withTraceId
  :: ( Monad m
     , Logging m LogData `In`    mods
     , WithTraceId     `NotIn` mods
     , ConsFDataList FData (WithTraceId : mods)
     )
  => TraceId -> EffT (WithTraceId : mods) es m a -> EffT mods es m a
withTraceId tid = effAddLogCat' (LogCat tid) . runWithTraceId (WithTraceIdRead tid)
{-# INLINE withTraceId #-}

-- | Assign new traceId using the provided TraceIdGen module
withNewTraceId
  :: ( MonadIO m
     , TraceIdGen      `In`    mods
     , Logging m LogData `In`    mods
     , WithTraceId     `NotIn` mods
     , ConsFDataList   FData   (WithTraceId : mods)
     )
  => EffT (WithTraceId : mods) es m a -> EffT mods es m a
withNewTraceId act = do
  newTidIO <- asksModule newTraceId
  newTrace <- liftIO     newTidIO
  withTraceId newTrace act
{-# INLINE withNewTraceId #-}

-- | Using a global XorShift random number generator for traceId
withRandomTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a -> EffT mods es m a
withRandomTraceIdGen act = do
  rng <- liftIO newRNG
  runTraceIdGen (TraceIdGenRead $ TraceId <$> uniformWord64FromRNG rng) act
{-# INLINE withRandomTraceIdGen #-}

-- | Using current time in microsecond precision for traceId
withTimeTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a -> EffT mods es m a
withTimeTraceIdGen act = do
  runTraceIdGen (TraceIdGenRead $ TraceId . floor . (*1000_000) <$> getPOSIXTime) act
{-# INLINE withTimeTraceIdGen #-}

-- | Using a simple counting number for traceId, starting from the provided number
withCountingTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => Word64  -- ^ starting count, e.g. you can use microsecond unix time
  -> EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withCountingTraceIdGen startCount act = do
  counter <- liftIO $ newTVarIO startCount
  let getNewTid = atomically $ do
        tid <- readTVar counter
        let !newTid = tid + 1
        writeTVar counter newTid
        return $ TraceId tid
  runTraceIdGen (TraceIdGenRead getNewTid) act
{-# INLINE withCountingTraceIdGen #-}

-- | Using current time in microsecond precision as the starting point for a counting traceId generator
withStartTimeCountingTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withStartTimeCountingTraceIdGen act = do
  startTime <- liftIO $ floor . (*1000_000) <$> getPOSIXTime
  withCountingTraceIdGen startTime act
{-# INLINE withStartTimeCountingTraceIdGen #-}
