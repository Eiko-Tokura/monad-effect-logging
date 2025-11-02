{-# LANGUAGE TemplateHaskell #-}
module Module.Logging.LogS
  ( module Module.Logging.LogS
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

logData :: (Monad m, In' c (Logging m LogS) mods) => LogS -> EffT' c mods es m ()
logData logd = logLog (Log [] logd)
{-# INLINE logData #-}

logLoc_ :: (Monad m, In' c (Logging m LogS) mods, IsLogCat subType) => ML.Loc -> subType -> ML.LogStr -> EffT' c mods es m ()
logLoc_ src subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogS & logMsg .~ msg & logLoc ?~ src))
{-# INLINE logLoc_ #-}

-- | Simple logging function, provide one log type and a LogStr message
log_ :: (MonadIO m, In' c (Logging m LogS) mods, IsLogCat subType) => subType -> ML.LogStr -> EffT' c mods es m ()
log_ subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogS & logMsg .~ msg))
{-# INLINE log_ #-}

-- | Log with multiple log types (wrapped in existantial constructor LogCat)
logs :: (MonadIO m, In' c (Logging m LogS) mods) => [LogCat] -> ML.LogStr -> EffT' c mods es m ()
logs logTypes msg = logLog (Log logTypes (mempty @LogS & logMsg .~ msg))
{-# INLINE logs #-}

-- | Template Haskell helper with location info
logTH :: (IsLogCat subType, TH.Lift subType) => subType -> TH.Q TH.Exp
logTH subType = [| logLoc_ $(TH.qLocation >>= TH.lift) $(TH.lift subType) |]
