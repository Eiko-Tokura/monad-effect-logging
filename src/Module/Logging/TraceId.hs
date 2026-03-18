{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}

module Module.Logging.TraceId
  ( -- * Types
    TraceId(..)
    -- * Modules
  , TraceIdGen
  , newTraceId
  , WithTraceId
  , traceId
    -- * Attaching Trace IDs
  , withTraceId
  , withNewTraceId
    -- * Trace ID Generators
  , withRandomTraceIdGen
  , withTimeTraceIdGen
  , withCountingTraceIdGen
  , withStartTimeCountingTraceIdGen
  ) where

import Control.Concurrent.STM
import Control.Monad.Effect
import Control.Monad.Logger (toLogStr)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.TypeList
import Data.Word
import Module.Logging
import Module.Logging.TraceId.XorShiftRNG
import Module.RS.QQ

newtype TraceId = TraceId {unTraceId :: Word64}
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance IsLogCat TraceId where
  logTypeDisplay (TraceId tid) = "TID=" <> toLogStr tid

[makeRModule__|
TraceIdGen
  newTraceId :: !(IO TraceId)
|]

[makeRModule__|
WithTraceId
  traceId :: !TraceId
|]

withTraceId
  :: forall doc m mods es a.
     ( Monad m
     , LogEffect m doc `In` mods
     , WithTraceId `NotIn` mods
     , ConsFDataList FData (WithTraceId : mods)
     )
  => TraceId
  -> EffT (WithTraceId : mods) es m a
  -> EffT mods es m a
withTraceId tid =
  effAddLogCat @doc (LogCat tid) . runWithTraceId (WithTraceIdRead tid)

withNewTraceId
  :: forall doc m mods es a.
     ( MonadIO m
     , TraceIdGen `In` mods
     , LogEffect m doc `In` mods
     , WithTraceId `NotIn` mods
     , ConsFDataList FData (WithTraceId : mods)
     )
  => EffT (WithTraceId : mods) es m a
  -> EffT mods es m a
withNewTraceId act = do
  mkTraceId <- asksModule newTraceId
  tid <- liftIO mkTraceId
  withTraceId @doc tid act

withRandomTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withRandomTraceIdGen act = do
  rng <- liftIO newRNG
  runTraceIdGen (TraceIdGenRead $ TraceId <$> uniformWord64FromRNG rng) act

withTimeTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withTimeTraceIdGen act =
  runTraceIdGen (TraceIdGenRead $ TraceId . floor . (* 1000_000) <$> getPOSIXTime) act

withCountingTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => Word64
  -> EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withCountingTraceIdGen startCount act = do
  counter <- liftIO $ newTVarIO startCount
  let nextTraceId = atomically $ do
        current <- readTVar counter
        let newValue = current + 1
        writeTVar counter newValue
        pure $ TraceId current
  runTraceIdGen (TraceIdGenRead nextTraceId) act

withStartTimeCountingTraceIdGen
  :: (MonadIO m, ConsFDataList FData (TraceIdGen : mods))
  => EffT (TraceIdGen : mods) es m a
  -> EffT mods es m a
withStartTimeCountingTraceIdGen act = do
  startTime <- liftIO $ floor . (* 1000_000) <$> getPOSIXTime
  withCountingTraceIdGen startTime act
