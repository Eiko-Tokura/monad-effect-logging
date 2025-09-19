{-# LANGUAGE TemplateHaskell, AllowAmbiguousTypes, OverloadedRecordDot, FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | We want a logger that supports open categories, open levels, open severities etc.
-- so that we can filter on different levels for different categories
--
-- finally we will also provide an interface for MonadLogger for compatibility
module Module.Logging where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Logger (Loc(..))
import Data.Fixed
import Data.Functor.Contravariant
import Data.Kind
import Data.Maybe
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
-- @
-- data DatabaseSubType = ConnectionPool | Query | Migration | Cursor deriving (Show, Eq)
-- @
--
-- you can then write instance
--
-- @
-- instance IsLogType DatabaseSubType where
--   severity _    = Nothing
--   logTypeName _ = "DB"
-- @
class Typeable sub => IsLogType (sub :: Type) where
  severity    :: sub -> Maybe LogSeverity
  -- | This is used for display only
  logTypeName :: sub -> ML.LogStr

-- | An exsitential type that wraps all log types, it is easy to define a new instance
data LogType where
  LogType :: forall sub. IsLogType sub => sub -> LogType

someSeverity :: LogType -> Maybe LogSeverity
someSeverity (LogType @a subType) = severity @a subType
{-# INLINE someSeverity #-}

someLogTypeName :: LogType -> ML.LogStr
someLogTypeName (LogType @a subType) = logTypeName @a subType
{-# INLINE someLogTypeName #-}

data Log a = Log
  { _logType    :: [LogType]
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

instance Semigroup LogData where
  l1 <> l2 = LogData
    { _logLoc = l1 ^. logLoc <|> l2 ^. logLoc
    , _logSource = l1 ^. logSource <|> l2 ^. logSource
    , _logMsg = l1 ^. logMsg <> l2 ^. logMsg
    }
  {-# INLINE (<>) #-}

instance Monoid LogData where
  mempty = LogData Nothing Nothing mempty
  {-# INLINE mempty #-}

-- | Some default log types, you can easily define your own
data    Debug = Debug
data    Info  = Info
data    Warn  = Warn
data    Error = Error
newtype Other = Other Text

instance IsLogType Debug where severity _ = Just 1; logTypeName _ = "DEBUG"
instance IsLogType Info  where severity _ = Just 2; logTypeName _ = "INFO"
instance IsLogType Warn  where severity _ = Just 3; logTypeName _ = "WARN"
instance IsLogType Error where severity _ = Just 4; logTypeName _ = "ERROR"
instance IsLogType Other where
  -- type LogSubType Other = Text
  severity _ = Just 2
  logTypeName (Other t) = "OTHER:" <> ML.toLogStr t

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
  { _runLogger :: Log a -> m ()
  }

makeLenses ''Logger

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
-- $ Some combinators for logger filtering
--
-- You can use these combinators to filter logs based on their types and severities.
-- Example:
--
-- @
-- localLogger
--   ( anyLogType (severityThat $ Predicate (>= 1))
--   . excludeLogType (isLogType @Database)
--   ) $ do
--     ...
-- @

-- | Locally modify the logger
localLogger :: (Monad m, Logging a `In` mods) => (Logger IO a -> Logger IO a) -> EffT mods es m b -> EffT mods es m b
localLogger f = localModule (\(LoggingRead logger) -> LoggingRead (f logger))
{-# INLINE localLogger #-}

-- | Locally modify the log
localLog :: (Monad m, Logging a `In` mods) => (Log a -> Log a) -> EffT mods es m c -> EffT mods es m c
localLog f = localLogger $ over runLogger (. f)
{-# INLINE localLog #-}

-- | Add a log type to the log
-- @
-- localLogger (addLogType $ LogType ConnectionPool) $ do
--   ...
-- @
addLogType :: LogType -> Logger m a -> Logger m a
addLogType t = over runLogger (. over logType (t:))
{-# INLINE addLogType #-}

filterLogTypes :: Applicative m => Predicate [LogType] -> Logger m a -> Logger m a
filterLogTypes p (Logger logger) = Logger $ \log' -> when (p.getPredicate $ log' ^. logType) $ logger log'
{-# INLINE filterLogTypes #-}

anyLogType :: Applicative m => Predicate LogType -> Logger m a -> Logger m a
anyLogType p = filterLogTypes (Predicate $ any p.getPredicate)
{-# INLINE anyLogType #-}

excludeLogType :: Applicative m => Predicate LogType -> Logger m a -> Logger m a
excludeLogType p = filterLogTypes (Predicate $ not . any p.getPredicate)
{-# INLINE excludeLogType #-}

severityThat :: Predicate LogSeverity -> Predicate LogType
severityThat = contramap (fromMaybe 0 . someSeverity)
{-# INLINE severityThat #-}

noSeverity :: Predicate LogType
noSeverity = Predicate (isNothing . someSeverity)
{-# INLINE noSeverity #-}

-- | use type applications to check if a log type is present
isLogType :: forall sub. IsLogType sub => Predicate LogType
isLogType = Predicate $ \(LogType (_ :: sub')) -> case eqT @sub @sub' of
  Just Refl -> True
  Nothing   -> False
{-# INLINE isLogType #-}

isLogSubType :: forall sub. IsLogType sub => Predicate sub -> Predicate LogType
isLogSubType p = Predicate $ \(LogType (subType :: sub')) -> case eqT @sub @sub' of
  Just Refl -> p.getPredicate subType
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
    liftIO $ _runLogger logger Log
      { _logType    = [mlLogLevelToLogType loglevel]
      , _logContent = LogData
          { _logLoc    = Just loc
          , _logSource = Just logsource
          , _logMsg    = ML.toLogStr msg
          }
      }
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, LoggingModule `In` mods) => ML.MonadLoggerIO (EffT mods es m) where
  askLoggerIO = queriesModule @LoggingModule $ (\f loc src lev str -> f
    $ Log [mlLogLevelToLogType lev]
    $ LogData
      { _logLoc    = Just loc
      , _logSource = Just src
      , _logMsg    = str
      }
    ) . _runLogger . logging
  {-# INLINE askLoggerIO #-}

-- | A compatibility function that works for the old MonadLogger instances
mlLogLevelToLogType :: ML.LogLevel -> LogType
mlLogLevelToLogType ML.LevelDebug     = LogType Debug
mlLogLevelToLogType ML.LevelInfo      = LogType Info
mlLogLevelToLogType ML.LevelWarn      = LogType Warn
mlLogLevelToLogType ML.LevelError     = LogType Error
mlLogLevelToLogType (ML.LevelOther t) = LogType (Other t)
{-# INLINE mlLogLevelToLogType #-}
