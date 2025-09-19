{-# LANGUAGE TemplateHaskell, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | We want a logger that supports open categories, open levels, open severities etc.
-- so that we can filter on different levels for different categories
--
-- finally we will also provide an interface for MonadLogger for compatibility
module Module.Logging where

import Control.Lens
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Logger (Loc(..))
import Data.Fixed
import Data.Kind
import Data.Text (Text)
import Data.Typeable
import qualified Control.Monad.Logger as ML

-- | so user can interpolate between levels easily
--
-- by default, Debug = 1, Info = 2, Warn = 3, Error = 4
-- you can use anything in between, with a precision of 1 decimal place
-- for example, 2.5 is between Info and Warn
type LogSeverity = Fixed E1

-- | So every module can have its own log type, for example
-- Database module can have `data Database` used as a log type
--
-- and have a subtype
-- data DatabaseSubType = ConnectionPool | Query | Migration | Cursor deriving (Show, Eq)
-- type LogSubType Database = DatabaseSubType
class Typeable a => IsLogType (a :: Type) where
  type LogSubType a :: Type
  type LogSubType a = ()
  severity :: LogSubType a -> LogSeverity
  logTypeName :: LogSubType a -> ML.LogStr

data SomeLogType where
  SomeLogType :: forall a. IsLogType a => Proxy a -> LogSubType a -> SomeLogType

someSeverity :: SomeLogType -> LogSeverity
someSeverity (SomeLogType (_ :: Proxy a) subType) = severity @a subType
{-# INLINE someSeverity #-}

someLogTypeName :: SomeLogType -> ML.LogStr
someLogTypeName (SomeLogType (_ :: Proxy a) subType) = logTypeName @a subType
{-# INLINE someLogTypeName #-}

data Log a = Log
  { _logType    :: [SomeLogType]
  , _logContent :: a
  } deriving (Functor)

data LogData = LogData
  { _logLoc    :: Maybe ML.Loc
  , _logSource :: Maybe ML.LogSource
  , _logMsg    :: ML.LogStr
  }

makeLenses ''Log
makeLenses ''LogData
makeLenses ''Loc

-- | Some default log types, you can easily define your own
data Debug
data Info
data Warn
data Error
data Other

instance IsLogType Debug where severity _ = 1; logTypeName _ = "DEBUG"
instance IsLogType Info  where severity _ = 2; logTypeName _ = "INFO"
instance IsLogType Warn  where severity _ = 3; logTypeName _ = "WARN"
instance IsLogType Error where severity _ = 4; logTypeName _ = "ERROR"
instance IsLogType Other where
  type LogSubType Other = Text
  severity _ = 2
  logTypeName t = "OTHER:" <> ML.toLogStr t

instance Semigroup a => Semigroup (Log a) where
  Log t1 c1 <> Log t2 c2 = Log (t1 <> t2) (c1 <> c2)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Log a) where
  mempty = Log [] mempty
  {-# INLINE mempty #-}

instance Applicative Log where
  pure = Log []
  {-# INLINE pure #-}
  Log t1 f <*> Log t2 a = Log (t1 <> t2) (f a)
  {-# INLINE (<*>) #-}

type Logger :: (Type -> Type) -> Type -> Type
newtype Logger m a = Logger
  { runLogger :: Log a -> m ()
  }

instance Applicative m => Semigroup (Logger m a) where
  Logger f <> Logger g = Logger $ \log' -> f log' *> g log'
  {-# INLINE (<>) #-}

instance Applicative m => Monoid (Logger m a) where
  mempty = Logger $ const $ pure ()
  {-# INLINE mempty #-}

instance Contravariant (Logger m) where
  contramap f (Logger g) = Logger (g . fmap f)
  {-# INLINE contramap #-}

--------------------------------------------------------------------------------

localLogger :: (Monad m, Logging a `In` mods) => (Logger IO a -> Logger IO a) -> EffT mods es m b -> EffT mods es m b
localLogger f = localModule (\(LoggingRead logger) -> LoggingRead (f logger))
{-# INLINE localLogger #-}

localLoggerContramap :: (Monad m, Logging a `In` mods) => (a -> a) -> EffT mods es m c -> EffT mods es m c
localLoggerContramap f = localLogger (contramap f)
{-# INLINE localLoggerContramap #-}

addLogType :: SomeLogType -> Log a -> Log a
addLogType t = over logType (t:)
{-# INLINE addLogType #-}

filterLogTypes :: Applicative m => ([SomeLogType] -> Bool) -> Logger m a -> Logger m a
filterLogTypes p (Logger logger) = Logger $ \log' -> when (p $ log' ^. logType) $ logger log'
{-# INLINE filterLogTypes #-}

anyLogType :: Applicative m => (SomeLogType -> Bool) -> Logger m a -> Logger m a
anyLogType p = filterLogTypes (any p)
{-# INLINE anyLogType #-}

excludeLogType :: Applicative m => (SomeLogType -> Bool) -> Logger m a -> Logger m a
excludeLogType p = filterLogTypes (not . any p)
{-# INLINE excludeLogType #-}

severityAtLeast :: Applicative m => LogSeverity -> Logger m a -> Logger m a
severityAtLeast s = anyLogType (\t -> someSeverity t >= s)
{-# INLINE severityAtLeast #-}

-- | use type applications to check if a log type is present
isLogType :: forall a. IsLogType a => SomeLogType -> Bool
isLogType (SomeLogType (Proxy :: Proxy b) _) = case eqT @a @b of
  Just Refl -> True
  Nothing   -> False
{-# INLINE isLogType #-}

isLogSubType :: forall a. IsLogType a => (LogSubType a -> Bool) -> SomeLogType -> Bool
isLogSubType p (SomeLogType (Proxy :: Proxy b) subType) = case eqT @a @b of
  Just Refl -> p subType
  Nothing   -> False
{-# INLINE isLogSubType #-}

-- | The Logging module type, a module `Logging a` provides logging capabilities for logs of type `a`
type Logging :: Type -> Type
data Logging a

instance Module (Logging (a :: Type)) where
  newtype ModuleRead  (Logging a) = LoggingRead { logging :: Logger IO a }
  data    ModuleState (Logging a) = LoggingState

type LoggingModule = Logging LogData

-- | This provides an interface to MonadLogger
instance (MonadIO m, LoggingModule `In` mods) => ML.MonadLogger (EffT mods es m) where
  monadLoggerLog loc logsource loglevel msg = do
    LoggingRead logger <- queryModule @LoggingModule
    liftIO $ runLogger logger Log
      { _logType    = [mlLogLevelToLogType loglevel]
      , _logContent = LogData
          { _logLoc    = Just loc
          , _logSource = Just logsource
          , _logMsg    = ML.toLogStr msg
          }
      }
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, LoggingModule `In` mods) => ML.MonadLoggerIO (EffT mods es m) where
  askLoggerIO = queriesModule @LoggingModule ((\f loc src lev str -> f $ Log [mlLogLevelToLogType lev] $ LogData
    { _logLoc    = Just loc
    , _logSource = Just src
    , _logMsg    = str
    }) . runLogger . logging)
  {-# INLINE askLoggerIO #-}

mlLogLevelToLogType :: ML.LogLevel -> SomeLogType
mlLogLevelToLogType ML.LevelDebug     = SomeLogType (Proxy @Debug) ()
mlLogLevelToLogType ML.LevelInfo      = SomeLogType (Proxy @Info ) ()
mlLogLevelToLogType ML.LevelWarn      = SomeLogType (Proxy @Warn ) ()
mlLogLevelToLogType ML.LevelError     = SomeLogType (Proxy @Error) ()
mlLogLevelToLogType (ML.LevelOther t) = SomeLogType (Proxy @Other) t
