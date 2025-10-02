{-# LANGUAGE QuasiQuotes #-}
-- | This module provides functionality for handling trace IDs in logging.
--
--  A trace Id is a unique identifier used to trace and correlate log entries across different parts of a system.
--  It is particularly useful in distributed systems for tracking requests as they propagate through various services.
module Module.Logging.TraceId where

import Control.Monad.Effect
import Control.Monad.Logger
import Data.TypeList
import Data.Word
import Module.Logging
import Module.RS.QQ
import Probability.Foundation.XorShiftRNG

newtype TraceId = TraceId { unTraceId :: Word64 } deriving (Eq, Ord, Show)

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

-- [makeRSModule__|
-- TraceIdGenPure
--   Read  rngUpdate :: Word64 -> Word64
--   Read  rngSplit  :: Word64 -> Word64
--   State rngState  :: !Word64
-- |]

withTraceId
  :: (Monad m, Logging LogData `In` mods, ConsFDataList FData (WithTraceId : mods), WithTraceId `NotIn` mods)
  => TraceId -> EffT (WithTraceId : mods) es m a -> EffT mods es m a
withTraceId tid = effAddLogCat' (LogCat tid) . runWithTraceId (WithTraceIdRead tid)
{-# INLINE withTraceId #-}

withNewTraceId
  :: ( MonadIO m
     , TraceIdGen      `In`    mods
     , Logging LogData `In`    mods
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
