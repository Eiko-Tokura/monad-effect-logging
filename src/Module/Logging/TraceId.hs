{-# LANGUAGE AllowAmbiguousTypes, QuasiQuotes, DeriveLift #-}
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
  :: forall log m mods es a.
     ( Monad m
     , Logging m log `In`    mods
     , WithTraceId   `NotIn` mods
     , ConsFDataList FData (WithTraceId : mods)
     )
  => TraceId -> EffT (WithTraceId : mods) es m a -> EffT mods es m a
withTraceId tid = effAddLogCat @log (LogCat tid) . runWithTraceId (WithTraceIdRead tid)
{-# INLINE withTraceId #-}

-- | Specialized for log = LogS
withTraceId'
  :: forall log m mods es a.
     ( Monad m
     , Logging m log `In`    mods
     , WithTraceId   `NotIn` mods
     , ConsFDataList FData (WithTraceId : mods)
     , log ~ LogS
     )
  => TraceId -> EffT (WithTraceId : mods) es m a -> EffT mods es m a
withTraceId' = withTraceId @log
{-# INLINE withTraceId' #-}

-- | Assign new traceId using the provided TraceIdGen module
withNewTraceId
  :: forall log m mods es a.
     ( MonadIO m
     , TraceIdGen     `In`    mods
     , Logging m log  `In`    mods
     , WithTraceId    `NotIn` mods
     , ConsFDataList  FData   (WithTraceId : mods)
     )
  => EffT (WithTraceId : mods) es m a -> EffT mods es m a
withNewTraceId act = do
  newTidIO <- asksModule newTraceId
  newTrace <- liftIO     newTidIO
  withTraceId @log newTrace act
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
