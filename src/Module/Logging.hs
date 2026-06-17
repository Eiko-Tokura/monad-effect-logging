{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Module.Logging
  ( -- * Core Types
    LogSeverity
  , IsLogCat(..)
  , LogCat(..)
  , someSeverity
  , someLogCatName
  , someLogCatDisplay
    -- * Log Event Model
  , LogEvent(..)
  , logEventCats
  , logEventPayload
  , LogWithSourceMeta(..)
  , logMetaLoc
  , logMetaSource
  , logMetaDoc
  , Logger(..)
  , runLogger
  , LogEffect
  , Logging
  , LoggingModule
    -- * Structured Log Documents
  , LogDoc
  , SomeShown
  , NamedColor(..)
  , Color(..)
  , Style(..)
  , defaultStyle
  , StyleMode(..)
  , DocRenderOptions(..)
  , defaultDocRenderOptions
  , renderLogDoc
  , logRaw
  , logShow
  , logFg
  , logBg
  , logBold
  , ToLog(..)
    -- * Default Log Categories
  , Debug(..)
  , Info(..)
  , Warn(..)
  , Error(..)
  , Other(..)
    -- * Logger Combinators
  , localLogger
  , localLogEvent
  , addLogCat
  , effAddLogCat
  , effAddLogCat'
  , filterLogCats
  , anyLogCat
  , excludeLogCat
  , severityThat
  , noSeverity
  , isLogCat
  , isLogSubType
  , isLogCatName
  , liftLogger
    -- * Logging Operations
  , emitLogEvent
  , log_
  , logLoc_
  , logs
  , logTH
  , logLocIO
  , logIO
  , logsIO
  , logTHIO
    -- * Logging directly with Logger
  , logEventWith
  , logLocWith_
  , logWith_
  , logsWith_
  , logTHWith
    -- * Running and Initialization
  , runLogEffect
  , withLiftLogger
  , defaultStringToLogSeverity
    -- * Compatibility
  , monadLoggerAdapter
  , mlLogLevelToLogCat
    -- * Re-exports
  , ModuleRead(..)
  , ModuleState(..)
  , ModuleInitData(..)
  , ModuleEvent(..)
  , module Data.Functor.Contravariant
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Effect
import Control.System
import Data.Fixed
import Data.Functor.Contravariant
import Data.Kind
import Data.List (intercalate)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Text.Read (readMaybe)
import qualified Control.Monad.Logger as ML
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

type LogSeverity = Fixed E1

class Typeable cat => IsLogCat (cat :: Type) where
  severity :: cat -> Maybe LogSeverity
  severity _ = Nothing

  logTypeName :: cat -> ML.LogStr
  {-# MINIMAL logTypeName #-}

  logTypeDisplay :: ML.LogStr -> LogDoc
  logTypeDisplay = DocRaw
  {-# INLINE logTypeDisplay #-}

instance IsLogCat Text where
  logTypeName = ML.toLogStr

data LogCat where
  LogCat :: forall cat. IsLogCat cat => cat -> LogCat

someSeverity :: LogCat -> Maybe LogSeverity
someSeverity (LogCat @cat x) = severity @cat x

someLogCatName :: LogCat -> ML.LogStr
someLogCatName (LogCat @cat x) = logTypeName @cat x

someLogCatDisplay :: LogCat -> LogDoc
someLogCatDisplay (LogCat @cat x) = logTypeDisplay @cat (logTypeName @cat x)

-- | Carries several log categories and a payload.
data LogEvent a = LogEvent
  { _logEventCats    :: [LogCat]
  , _logEventPayload :: a
  }
  deriving (Functor)

data LogWithSourceMeta a = LogWithSourceMeta
  { _logMetaLoc    :: Maybe ML.Loc
  , _logMetaSource :: Maybe ML.LogSource
  , _logMetaDoc    :: a
  }
  deriving (Functor)

data SomeShown where
  SomeShown :: Show a => a -> SomeShown

data NamedColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Show)

data Color
  = DefaultColor
  | Named !NamedColor
  | RGB   !Word8 !Word8 !Word8
  deriving (Eq, Show)

class IsLogColor c where
  toLogColor :: c -> Color

instance IsLogColor Color where
  toLogColor = id

instance IsLogColor NamedColor where
  toLogColor = Named

instance IsLogColor (Word8, Word8, Word8) where
  toLogColor (r, g, b) = RGB r g b

class ToLog a where
  toLog :: a -> LogDoc

instance ToLog LogDoc where
  toLog = id

instance ToLog Text where
  toLog = DocRaw . ML.toLogStr

instance ToLog String where
  toLog = DocRaw . ML.toLogStr

data Style = Style
  { styleFg   :: Maybe Color
  , styleBg   :: Maybe Color
  , styleBold :: Bool
  }
  deriving (Eq, Show)

defaultStyle :: Style
defaultStyle =
  Style
    { styleFg   = Nothing
    , styleBg   = Nothing
    , styleBold = False
    }

data LogDoc
  = DocEmpty
  | DocRaw    ML.LogStr
  | DocShown  SomeShown
  | DocStyled Style  LogDoc
  | DocAppend LogDoc LogDoc

data StyleMode
  = NoStyles
  | AnsiStyles
  deriving (Eq, Show)

data DocRenderOptions = DocRenderOptions
  { docRenderShow      :: forall a. Show a => a -> ML.LogStr
  , docRenderStyleMode :: StyleMode
  }

defaultDocRenderOptions :: DocRenderOptions
defaultDocRenderOptions =
  DocRenderOptions
    { docRenderShow      = ML.toLogStr . show
    , docRenderStyleMode = NoStyles
    }

makeLenses ''LogEvent
makeLenses ''LogWithSourceMeta

instance Semigroup a => Semigroup (LogEvent a) where
  LogEvent catsA payloadA <> LogEvent catsB payloadB =
    LogEvent (catsA <> catsB) (payloadA <> payloadB)

instance Monoid a => Monoid (LogEvent a) where
  mempty = LogEvent [] mempty

instance Applicative LogEvent where
  pure = LogEvent []
  LogEvent catsF f <*> LogEvent catsA a = LogEvent (catsF <> catsA) (f a)

instance Monad LogEvent where
  LogEvent catsA a >>= f =
    let LogEvent catsB b = f a
     in LogEvent (catsA <> catsB) b

instance Semigroup a => Semigroup (LogWithSourceMeta a) where
  metaA <> metaB =
    LogWithSourceMeta
      { _logMetaLoc    = _logMetaLoc    metaA <|> _logMetaLoc    metaB
      , _logMetaSource = _logMetaSource metaA <|> _logMetaSource metaB
      , _logMetaDoc    = _logMetaDoc    metaA <>  _logMetaDoc    metaB
      }

instance Monoid a => Monoid (LogWithSourceMeta a) where
  mempty = LogWithSourceMeta Nothing Nothing mempty

instance IsString LogDoc where
  fromString = DocRaw . ML.toLogStr

instance Semigroup LogDoc where
  (<>) = DocAppend

instance Monoid LogDoc where
  mempty = DocEmpty

data Debug    = Debug      deriving TH.Lift
data Info     = Info       deriving TH.Lift
data Warn     = Warn       deriving TH.Lift
data Error    = Error      deriving TH.Lift
newtype Other = Other Text deriving TH.Lift

instance IsLogCat Debug where
  severity _ = Just 1
  logTypeName _ = "DEBUG"
  logTypeDisplay = logFg Blue . DocRaw

instance IsLogCat Info where
  severity _ = Just 2
  logTypeName _ = "INFO"
  logTypeDisplay = logFg Green . DocRaw

instance IsLogCat Warn where
  severity _ = Just 3
  logTypeName _ = "WARN"
  logTypeDisplay = logFg Yellow . DocRaw

instance IsLogCat Error where
  severity _ = Just 4
  logTypeName _ = "ERROR"
  logTypeDisplay = logFg Red . DocRaw

instance IsLogCat Other where
  severity _ = Just 2
  logTypeName (Other t) = "OTHER:" <> ML.toLogStr t

type Logger :: (Type -> Type) -> Type -> Type
newtype Logger m a = Logger
  { _runLogger :: LogEvent a -> m ()
  }

runLogger :: Logger m a -> LogEvent a -> m ()
runLogger = _runLogger

liftLogger :: (m () -> n ()) -> Logger m a -> Logger n a
liftLogger nat (Logger f) = Logger (nat . f)

instance Applicative m => Semigroup (Logger m a) where
  Logger f <> Logger g = Logger $ \entry -> f entry *> g entry

instance Applicative m => Monoid (Logger m a) where
  mempty = Logger $ const $ pure ()

instance Contravariant (Logger m) where
  contramap f (Logger g) = Logger (g . fmap f)

logRaw :: ML.LogStr -> LogDoc
logRaw = DocRaw

logShow :: Show a => a -> LogDoc
logShow = DocShown . SomeShown

logFg :: IsLogColor color => color -> LogDoc -> LogDoc
logFg color = DocStyled defaultStyle {styleFg = Just $ toLogColor color}

logBg :: IsLogColor color => color -> LogDoc -> LogDoc
logBg color = DocStyled defaultStyle {styleBg = Just $ toLogColor color}

logBold :: LogDoc -> LogDoc
logBold = DocStyled defaultStyle {styleBold = True}

renderLogDoc :: DocRenderOptions -> LogDoc -> ML.LogStr
renderLogDoc opts = case docRenderStyleMode opts of
  NoStyles -> goPlain
  AnsiStyles -> goAnsi defaultStyle
  where
    goPlain DocEmpty                 = mempty
    goPlain (DocRaw str)             = str
    goPlain (DocShown (SomeShown x)) = docRenderShow opts x
    goPlain (DocStyled _ doc)        = goPlain doc
    goPlain (DocAppend a b)          = goPlain a <> goPlain b

    goAnsi _       DocEmpty                 = mempty
    goAnsi _       (DocRaw str)             = str
    goAnsi _       (DocShown (SomeShown x)) = docRenderShow opts x
    goAnsi current (DocAppend a b)          = goAnsi current a <> goAnsi current b
    goAnsi current (DocStyled style doc)    =
      let merged = mergeStyle current style
       in ansiForStyle merged <> goAnsi merged doc <> ansiForStyle current

mergeStyle :: Style -> Style -> Style
mergeStyle outer inner =
  Style
    { styleFg   = styleFg   inner <|> styleFg   outer
    , styleBg   = styleBg   inner <|> styleBg   outer
    , styleBold = styleBold outer ||  styleBold inner
    }

ansiForStyle :: Style -> ML.LogStr
ansiForStyle style =
  ML.toLogStr $ "\ESC[" <> mconcat (intercalate [";"] (pure <$> codes)) <> "m"
  where
    codes =
      let baseCodes =
            concat
              [ maybe [] colorToFgCodes (styleFg style)
              , maybe [] colorToBgCodes (styleBg style)
              , ["1" | styleBold style]
              ]
       in if null baseCodes then ["0"] else baseCodes

colorToFgCodes :: Color -> [ML.LogStr]
colorToFgCodes DefaultColor  = ["39"]
colorToFgCodes (Named color) = [lshow $ namedColorCode color]
colorToFgCodes (RGB r g b)   = ["38", "2", lshow r, lshow g, lshow b]

colorToBgCodes :: Color -> [ML.LogStr]
colorToBgCodes DefaultColor  = ["49"]
colorToBgCodes (Named color) = [lshow $ namedColorCode color + 10]
colorToBgCodes (RGB r g b)   = ["48", "2", lshow r, lshow g, lshow b]

lshow :: Show a => a -> ML.LogStr
lshow = ML.toLogStr . show
{-# INLINE lshow #-}

namedColorCode :: NamedColor -> Int
namedColorCode = \case
  Black   -> 30
  Red     -> 31
  Green   -> 32
  Yellow  -> 33
  Blue    -> 34
  Magenta -> 35
  Cyan    -> 36
  White   -> 37

localLogger
  :: forall a c m mods es b.
     (Monad m, In' c (LogEffect m a) mods)
  => (Logger m (LogWithSourceMeta a) -> Logger m (LogWithSourceMeta a))
  -> EffT' c mods es m b
  -> EffT' c mods es m b
localLogger f =
  localModule (\(LogEffectRead logger) -> LogEffectRead (f logger))

localLogEvent
  :: forall a m mods es c.
     (Monad m, LogEffect m a `In` mods)
  => (LogEvent (LogWithSourceMeta a) -> LogEvent (LogWithSourceMeta a))
  -> EffT mods es m c
  -> EffT mods es m c
localLogEvent f = localLogger $ \(Logger g) -> Logger (g . f)

addLogCat :: LogCat -> Logger m a -> Logger m a
addLogCat cat (Logger g) =
  Logger $ \entry -> g entry {_logEventCats = cat : _logEventCats entry}

effAddLogCat
  :: forall a c mods es m b.
     (Monad m, In' c (LogEffect m a) mods)
  => LogCat
  -> EffT' c mods es m b
  -> EffT' c mods es m b
effAddLogCat cat = localLogger @a (addLogCat cat)

effAddLogCat'
  :: forall c mods es m b.
     (Monad m, In' c (LogEffect m LogDoc) mods)
  => LogCat
  -> EffT' c mods es m b
  -> EffT' c mods es m b
effAddLogCat' = effAddLogCat @LogDoc

filterLogCats :: Applicative m => Predicate [LogCat] -> Logger m a -> Logger m a
filterLogCats p (Logger logger) =
  Logger $ \entry ->
    when (getPredicate p $ _logEventCats entry) $
      logger entry

anyLogCat :: Applicative m => Predicate LogCat -> Logger m a -> Logger m a
anyLogCat p = filterLogCats (Predicate $ any (getPredicate p))

excludeLogCat :: Applicative m => Predicate LogCat -> Logger m a -> Logger m a
excludeLogCat p = filterLogCats (Predicate $ not . any (getPredicate p))

severityThat :: Predicate LogSeverity -> Predicate LogCat
severityThat = contramap (fromMaybe 0 . someSeverity)

noSeverity :: Predicate LogCat
noSeverity = Predicate (isNothing . someSeverity)

-- | Use with type applications
isLogCat :: forall cat. IsLogCat cat => Predicate LogCat
isLogCat =
  Predicate $ \(LogCat (_ :: cat')) -> case eqT @cat @cat' of
    Just Refl -> True
    Nothing -> False

isLogSubType :: forall cat. IsLogCat cat => Predicate cat -> Predicate LogCat
isLogSubType p =
  Predicate $ \(LogCat (x :: cat')) -> case eqT @cat @cat' of
    Just Refl -> getPredicate p x
    Nothing -> False

isLogCatName :: ML.ToLogStr n => n -> Predicate LogCat
isLogCatName name =
  Predicate $ \logCat -> someLogCatName logCat == ML.toLogStr name

type LogEffect :: (Type -> Type) -> Type -> Type
data LogEffect m a

type Logging = LogEffect

type LoggingModule = LogEffect IO LogDoc

withLiftLogger
  :: forall m n c a mods es b.
     ( Monad m
     , In' c (LogEffect m a) (LogEffect m a : mods)
     , ConsFDataList c (LogEffect n a : mods)
     , ConsFDataList c (LogEffect m a : mods)
     )
  => (forall x. m x -> n x)
  -> EffT' c (LogEffect n a : mods) es m b
  -> EffT' c (LogEffect m a : mods) es m b
withLiftLogger lifter act = do
  LogEffectRead logger <- askModule @(LogEffect m a)
  embedMods $ runEffTOuter_ (LogEffectRead $ liftLogger lifter logger) LogEffectState act

instance Module (LogEffect m a) where
  newtype ModuleRead (LogEffect m a) = LogEffectRead
    { logging :: Logger m (LogWithSourceMeta a)
    }
  data ModuleState (LogEffect m a) = LogEffectState

runLogEffect
  :: (ConsFDataList c (LogEffect m a : mods), Monad m)
  => Logger m (LogWithSourceMeta a)
  -> EffT' c (LogEffect m a : mods) es m b
  -> EffT' c mods es m b
runLogEffect logger = runEffTOuter_ (LogEffectRead logger) LogEffectState

instance SystemModule (LogEffect m a) where
  data ModuleInitData (LogEffect m a) = LogEffectInitData
    { loggerInitBase       :: Logger m (LogWithSourceMeta a)
    , loggerInitCleanup    :: Maybe (m ())
    , loggerInitTransform  :: Logger m (LogWithSourceMeta a) -> Logger m (LogWithSourceMeta a)
    , loggerInitSeverity   :: Maybe LogSeverity
    }
  data ModuleEvent (LogEffect m a) = LogEffectEvent

instance Loadable c (LogEffect IO a) mods ies where
  withModule initData act =
    let transformedLogger = loggerInitTransform initData (loggerInitBase initData)
        baseLogger = case loggerInitSeverity initData of
          Nothing -> transformedLogger
          Just sev ->
            anyLogCat (severityThat $ Predicate (>= sev)) transformedLogger
        runAction = runEffTOuter_ (LogEffectRead baseLogger) LogEffectState act
     in case loggerInitCleanup initData of
          Nothing -> runAction
          Just cleanup -> bracketEffT (pure ()) (\_ -> liftIO cleanup) (const runAction)

instance EventLoop c (LogEffect m a) mods es

emitLogEvent
  :: forall a c mods es m.
     (Monad m, In' c (LogEffect m a) mods)
  => LogEvent (LogWithSourceMeta a)
  -> EffT' c mods es m ()
emitLogEvent entry = do
  action <- asksModule @(LogEffect m a) (runLogger . logging)
  lift $ action entry

logEventWith :: Logger m a -> LogEvent a -> m ()
logEventWith = _runLogger

logLoc_
  :: forall c mods es m cat.
     (Monad m, In' c (LogEffect m LogDoc) mods, IsLogCat cat)
  => ML.Loc
  -> cat
  -> LogDoc
  -> EffT' c mods es m ()
logLoc_ loc cat doc =
  emitLogEvent $
    LogEvent
      { _logEventCats = [LogCat cat]
      , _logEventPayload =
          LogWithSourceMeta
            { _logMetaLoc    = Just loc
            , _logMetaSource = Nothing
            , _logMetaDoc    = doc
            }
      }

logLocWith_ :: IsLogCat cat => Logger m (LogWithSourceMeta LogDoc) -> ML.Loc -> cat -> LogDoc -> m ()
logLocWith_ logger loc cat doc = logEventWith logger
  LogEvent
    { _logEventCats = [LogCat cat]
    , _logEventPayload =
        LogWithSourceMeta
          { _logMetaLoc    = Just loc
          , _logMetaSource = Nothing
          , _logMetaDoc    = doc
          }
    }

logWith_ :: IsLogCat cat => Logger m (LogWithSourceMeta LogDoc) -> cat -> LogDoc -> m ()
logWith_ logger cat doc = logEventWith logger
  LogEvent
    { _logEventCats = [LogCat cat]
    , _logEventPayload =
        LogWithSourceMeta
          { _logMetaLoc    = Nothing
          , _logMetaSource = Nothing
          , _logMetaDoc    = doc
          }
    }

logsWith_ :: Logger m (LogWithSourceMeta LogDoc) -> [LogCat] -> LogDoc -> m ()
logsWith_ logger cats doc = logEventWith logger
  LogEvent
    { _logEventCats = cats
    , _logEventPayload =
        LogWithSourceMeta
          { _logMetaLoc    = Nothing
          , _logMetaSource = Nothing
          , _logMetaDoc    = doc
          }
    }

log_
  :: forall c mods es m cat.
     (Monad m, In' c (LogEffect m LogDoc) mods, IsLogCat cat)
  => cat
  -> LogDoc
  -> EffT' c mods es m ()
log_ cat doc =
  emitLogEvent $
    LogEvent
      { _logEventCats = [LogCat cat]
      , _logEventPayload =
          LogWithSourceMeta
            { _logMetaLoc    = Nothing
            , _logMetaSource = Nothing
            , _logMetaDoc    = doc
            }
      }

logs
  :: forall c mods es m.
     (Monad m, In' c (LogEffect m LogDoc) mods)
  => [LogCat]
  -> LogDoc
  -> EffT' c mods es m ()
logs cats doc =
  emitLogEvent $
    LogEvent
      { _logEventCats = cats
      , _logEventPayload =
          LogWithSourceMeta
            { _logMetaLoc    = Nothing
            , _logMetaSource = Nothing
            , _logMetaDoc    = doc
            }
      }

logTH :: (IsLogCat cat, TH.Lift cat) => cat -> TH.Q TH.Exp
logTH cat = [| logLoc_ $(TH.qLocation >>= TH.lift) $(TH.lift cat) |]

-- | A TH helper for 'logLocWith_' that captures the call site 'ML.Loc' at
-- compile time. Splices into a function expecting the 'Logger' and the
-- 'LogDoc' payload:
--
-- @
-- \logger doc -> logLocWith_ logger '<loc> '<cat> doc
-- @
logTHWith :: (IsLogCat cat, TH.Lift cat) => cat -> TH.Q TH.Exp
logTHWith cat = [| logLocWith_ $(TH.qLocation >>= TH.lift) $(TH.lift cat) |]

logLocIO
  :: forall c mods es m cat.
     (MonadIO m, In' c (LogEffect IO LogDoc) mods, IsLogCat cat)
  => ML.Loc
  -> cat
  -> LogDoc
  -> EffT' c mods es m ()
logLocIO loc cat = baseTransform liftIO . logLoc_ loc cat

logIO
  :: forall c mods es m cat.
     (MonadIO m, In' c (LogEffect IO LogDoc) mods, IsLogCat cat)
  => cat
  -> LogDoc
  -> EffT' c mods es m ()
logIO cat = baseTransform liftIO . log_ cat

logsIO
  :: forall c mods es m.
     (MonadIO m, In' c (LogEffect IO LogDoc) mods)
  => [LogCat]
  -> LogDoc
  -> EffT' c mods es m ()
logsIO cats = baseTransform liftIO . logs cats

logTHIO :: (IsLogCat cat, TH.Lift cat) => cat -> TH.Q TH.Exp
logTHIO cat = [| baseTransform liftIO . logLoc_ @LogDoc $(TH.qLocation >>= TH.lift) $(TH.lift cat) |]

defaultStringToLogSeverity :: String -> Either Text LogSeverity
defaultStringToLogSeverity = \case
  "Debug" -> Right 1
  "Info"  -> Right 2
  "Warn"  -> Right 3
  "Error" -> Right 4
  other ->
    maybe
      (Left "Invalid LogLevel, must be one of 'Debug', 'Info', 'Warn', 'Error', or a number between 0 and 10 with a precision of 1 decimal place")
      Right
      (readMaybe other)

monadLoggerAdapter
  :: Logger m (LogWithSourceMeta LogDoc)
  -> ML.Loc
  -> ML.LogSource
  -> ML.LogLevel
  -> ML.LogStr
  -> m ()
monadLoggerAdapter logger loc src lev msg =
  runLogger logger $
    LogEvent
      { _logEventCats = [mlLogLevelToLogCat lev]
      , _logEventPayload =
          LogWithSourceMeta
            { _logMetaLoc    = Just loc
            , _logMetaSource = Just src
            , _logMetaDoc    = logRaw msg
            }
      }

instance (Monad m, In' c (LogEffect m LogDoc) mods) => ML.MonadLogger (EffT' c mods es m) where
  monadLoggerLog loc src lev msg = do
    LogEffectRead logger <- queryModule @(LogEffect m LogDoc)
    lift $ monadLoggerAdapter logger loc src lev (ML.toLogStr msg)

instance (m ~ IO, In' c (LogEffect m LogDoc) mods) => ML.MonadLoggerIO (EffT' c mods es m) where
  askLoggerIO =
    queriesModule @(LogEffect m LogDoc) $
      (\f loc src lev str ->
         f $
           LogEvent
             { _logEventCats = [mlLogLevelToLogCat lev]
             , _logEventPayload =
                 LogWithSourceMeta
                   { _logMetaLoc    = Just loc
                   , _logMetaSource = Just src
                   , _logMetaDoc    = logRaw str
                   }
             }
      )
        . runLogger
        . logging

mlLogLevelToLogCat :: ML.LogLevel -> LogCat
mlLogLevelToLogCat = \case
  ML.LevelDebug   -> LogCat Debug
  ML.LevelInfo    -> LogCat Info
  ML.LevelWarn    -> LogCat Warn
  ML.LevelError   -> LogCat Error
  ML.LevelOther t -> LogCat (Other t)
