{-# LANGUAGE TemplateHaskell #-}
module Module.Logging.LogS
  (
  -- * Log value builders and renderers
    toLog
  , logShow
  -- * Logging module types
  , LogS
  , LoggingModule
  -- * General logging utilities
  , log_
  , logLoc_
  , logs
  , logTH
  , logS
  -- * MonadIO specific versions
  , logLocIO
  , logIO
  , logsIO
  , logTHIO
  -- * Re-export
  , ML.LogStr
  ) where

import qualified Control.Monad.Logger as ML
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Module.Logging
import Control.Monad.Effect
import Control.Lens

toLog :: (ML.ToLogStr a) => a -> ML.LogStr
toLog = ML.toLogStr
{-# INLINE toLog #-}

logShow :: Show a => a -> ML.LogStr
logShow = ML.toLogStr . show
{-# INLINE logShow #-}

logS :: (Monad m, In' c (Logging m LogS) mods) => LogS -> EffT' c mods es m ()
logS logd = logLog (Log [] logd)
{-# INLINE logS #-}

logLoc_ :: (Monad m, In' c (Logging m LogS) mods, IsLogCat subType) => ML.Loc -> subType -> ML.LogStr -> EffT' c mods es m ()
logLoc_ src subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogS & logMsg .~ msg & logLoc ?~ src))
{-# INLINE logLoc_ #-}

-- | Simple logging function, provide one log type and a LogStr message
log_ :: (Monad m, In' c (Logging m LogS) mods, IsLogCat subType) => subType -> ML.LogStr -> EffT' c mods es m ()
log_ subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogS & logMsg .~ msg))
{-# INLINE log_ #-}

-- | Log with multiple log types (wrapped in existantial constructor LogCat)
logs :: (Monad m, In' c (Logging m LogS) mods) => [LogCat] -> ML.LogStr -> EffT' c mods es m ()
logs logTypes msg = logLog (Log logTypes (mempty @LogS & logMsg .~ msg))
{-# INLINE logs #-}

-- | Template Haskell helper with location info
logTH :: (IsLogCat subType, TH.Lift subType) => subType -> TH.Q TH.Exp
logTH subType = [| logLoc_ $(TH.qLocation >>= TH.lift) $(TH.lift subType) |]

---------- Monad IO specific versions ----------

logLocIO :: (MonadIO m, In' c (Logging IO LogS) mods, IsLogCat subType) => ML.Loc -> subType -> ML.LogStr -> EffT' c mods es m ()
logLocIO src subTypeType = baseTransform liftIO . logLoc_ src subTypeType
{-# INLINE logLocIO #-}

logIO :: (MonadIO m, In' c (Logging IO LogS) mods, IsLogCat subType) => subType -> ML.LogStr -> EffT' c mods es m ()
logIO subTypeType = baseTransform liftIO . log_ subTypeType
{-# INLINE logIO #-}

logsIO :: (MonadIO m, In' c (Logging IO LogS) mods) => [LogCat] -> ML.LogStr -> EffT' c mods es m ()
logsIO logTypes = baseTransform liftIO . logs logTypes
{-# INLINE logsIO #-}

-- | Template Haskell helper with location info, with MonadIO
logTHIO :: (IsLogCat subType, TH.Lift subType) => subType -> TH.Q TH.Exp
logTHIO subType = [| baseTransform liftIO . logLoc_ @IO $(TH.qLocation >>= TH.lift) $(TH.lift subType) |]
