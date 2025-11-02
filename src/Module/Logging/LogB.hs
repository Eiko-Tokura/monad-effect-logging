-- | Module      : Module.Logging.LogVal
--   Description : Log value builders and renderers. This module provides a way to build log messages separately from rendering them, allowing for flexible logging rendering strategies (e.g. with console colors, JSON formatting, etc.).
{-# LANGUAGE TemplateHaskell #-}
module Module.Logging.LogB
  ( LogVals
  , LogBuilder
  , LogRenderer
  , toLog
  , logShow
  , renderUsing

  , LogB
  , LoggingModuleB
  , log_
  , logLoc_
  , logs
  , logTH
  ) where

import Data.String (IsString(..))
import qualified Control.Monad.Logger as ML
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Module.Logging
import Control.Monad.Effect
import Control.Lens

-- | Note on LogStr from monad-logger:
--   It is just Data.ByteString.Builder.Builder, but with extra length information for buffering.
data LogVal where
  LogValStr :: ML.LogStr   -> LogVal
  LogShow   :: Show a => a -> LogVal

type LogVals = [LogVal]

type LogBuilder   = LogVals    -> LogVals
type LogRenderer  = LogBuilder -> ML.LogStr

instance IsString LogVal where
  fromString = LogValStr . fromString
  {-# INLINE fromString #-}

instance IsString LogBuilder where
  fromString s = (LogValStr (fromString s) :)
  {-# INLINE fromString #-}
instance {-# OVERLAPPING #-} Semigroup LogBuilder where
  (<>) = (.)
  {-# INLINE (<>) #-}
instance {-# OVERLAPPING #-} Monoid LogBuilder where
  mempty = id
  {-# INLINE mempty #-}

-- | Use this function together with OverloadedStrings and <> to log your messages
logShow :: Show a => a -> LogBuilder
logShow v = (LogShow v :)
{-# INLINE logShow #-}

renderUsing :: (forall a. Show a => a -> ML.LogStr) -> LogRenderer
renderUsing func vals = mconcat @ML.LogStr $ map renderVal (vals [])
  where renderVal (LogValStr ls) = ls
        renderVal (LogShow   v ) = func v

type LogB = LogMsg LogBuilder

type LoggingModuleB = Logging IO LogB

toLog :: (ML.ToLogStr a) => a -> LogBuilder
toLog = (:) . LogValStr . ML.toLogStr
{-# INLINE toLog #-}

logLoc_ :: (Monad m, In' c (Logging m LogB) mods, IsLogCat subType) => ML.Loc -> subType -> LogBuilder -> EffT' c mods es m ()
logLoc_ src subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogB & logMsg .~ msg & logLoc ?~ src))
{-# INLINE logLoc_ #-}

-- | Simple logging function, provide one log type and a LogStr message
log_ :: (MonadIO m, In' c (Logging m LogB) mods, IsLogCat subType) => subType -> LogBuilder -> EffT' c mods es m ()
log_ subTypeType msg = logLog (Log [LogCat subTypeType] (mempty @LogB & logMsg .~ msg))
{-# INLINE log_ #-}

-- | Log with multiple log types (wrapped in existantial constructor LogCat)
logs :: (MonadIO m, In' c (Logging m LogB) mods) => [LogCat] -> LogBuilder -> EffT' c mods es m ()
logs logTypes msg = logLog (Log logTypes (mempty @LogB & logMsg .~ msg))
{-# INLINE logs #-}

-- | Template Haskell helper with location info
logTH :: (IsLogCat subType, TH.Lift subType) => subType -> TH.Q TH.Exp
logTH subType = [| logLoc_ $(TH.qLocation >>= TH.lift) $(TH.lift subType) |]
