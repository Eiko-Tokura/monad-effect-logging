{-# LANGUAGE TemplateHaskell, AllowAmbiguousTypes, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | We want a logger that supports open categories, open levels, open severities etc.
-- so that we can filter on different levels for different categories
--
-- finally we will also provide an interface for MonadLogger for compatibility
module Module.Logging
  ( module Module.Logging
  , module Data.Functor.Contravariant
  ) where

import Control.Applicative
import Control.Lens
import Control.System
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

import System.Environment
import Text.Read (readMaybe)

-- | so user can interpolate between levels easily
--
-- by default, Debug = 1, Info = 2, Warn = 3, Error = 4
-- you can use anything in between, with a precision of 1 decimal place
-- for example, 2.5 is between Info and Warn
type LogSeverity = Fixed E1

-- | So every module can have its own logging category type, for example
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
-- instance IsLogCat DatabaseSubType where
--   severity _    = Nothing
--   logTypeDisplay _ = "DB"
-- @
class Typeable sub => IsLogCat (sub :: Type) where
  severity :: sub -> Maybe LogSeverity
  severity _ = Nothing
  {-# INLINE severity #-}
  -- | This is used for display only
  logTypeDisplay :: sub -> ML.LogStr
  {-# MINIMAL logTypeDisplay #-}

instance IsLogCat Text where
  logTypeDisplay = ML.toLogStr
  {-# INLINE logTypeDisplay #-}

-- | An exsitential type that wraps all logging categories, it is easy to define a new instance
data LogCat where
  LogCat :: forall sub. IsLogCat sub => sub -> LogCat

someSeverity :: LogCat -> Maybe LogSeverity
someSeverity (LogCat @a subType) = severity @a subType
{-# INLINE someSeverity #-}

someLogCatName :: LogCat -> ML.LogStr
someLogCatName (LogCat @a subType) = logTypeDisplay @a subType
{-# INLINE someLogCatName #-}

data Log a = Log
  { _logType    :: [LogCat]
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

instance IsLogCat Debug where severity _ = Just 1; logTypeDisplay _ = "DEBUG"
instance IsLogCat Info  where severity _ = Just 2; logTypeDisplay _ = "INFO"
instance IsLogCat Warn  where severity _ = Just 3; logTypeDisplay _ = "WARN"
instance IsLogCat Error where severity _ = Just 4; logTypeDisplay _ = "ERROR"
instance IsLogCat Other where
  severity _ = Just 2
  logTypeDisplay (Other t) = "OTHER:" <> ML.toLogStr t

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

instance Monad Log where
  (Log t a) >>= f =
    let Log t' a' = f a
    in Log (t <> t') a'
  {-# INLINE (>>=) #-}

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
--   ( anyLogCat (severityThat $ Predicate (>= 1))
--   . excludeLogCat (isLogCat @Database)
--   ) $ do
--     ...
-- @

-- | Locally modify the logger
localLogger :: forall a c m mods es b. (Monad m, In' c (Logging a) mods) => (Logger IO a -> Logger IO a) -> EffT' c mods es m b -> EffT' c mods es m b
localLogger f = localModule (\(LoggingRead logger) -> LoggingRead (f logger))
{-# INLINE localLogger #-}

-- | Locally modify the log
localLog :: forall a m mods es c. (Monad m, Logging a `In` mods) => (Log a -> Log a) -> EffT mods es m c -> EffT mods es m c
localLog f = localLogger $ over runLogger (. f)
{-# INLINE localLog #-}

-- | Add a log category to the log
-- @
-- localLogger (addLogCat $ LogCat ConnectionPool) $ do
--   ...
-- @
addLogCat :: LogCat -> Logger m a -> Logger m a
addLogCat t = over runLogger (. over logType (t:))
{-# INLINE addLogCat #-}

-- | Add a log category to the log in EffT
-- @
-- effAddLogCat @LogData (LogCat ConnectionPool) $ do
--   ...
-- @
effAddLogCat :: forall a c mods es m b. (Monad m, In' c (Logging a) mods) => LogCat -> EffT' c mods es m b -> EffT' c mods es m b
effAddLogCat logCat = localLogger @a (addLogCat logCat)
{-# INLINE effAddLogCat #-}

-- | Add a log category to the log in EffT (defaulting to In (Logging LogData) mods)
-- @
-- effAddLogCat' (LogCat ConnectionPool) $ do
--   ...
-- @
effAddLogCat' :: forall c mods es m b. (Monad m, In' c (Logging LogData) mods) => LogCat -> EffT' c mods es m b -> EffT' c mods es m b
effAddLogCat' logCat = localLogger @LogData (addLogCat logCat)
{-# INLINE effAddLogCat' #-}

filterLogCats :: Applicative m => Predicate [LogCat] -> Logger m a -> Logger m a
filterLogCats p (Logger logger) = Logger $ \log' -> when (p.getPredicate $ log' ^. logType) $ logger log'
{-# INLINE filterLogCats #-}

anyLogCat :: Applicative m => Predicate LogCat -> Logger m a -> Logger m a
anyLogCat p = filterLogCats (Predicate $ any p.getPredicate)
{-# INLINE anyLogCat #-}

excludeLogCat :: Applicative m => Predicate LogCat -> Logger m a -> Logger m a
excludeLogCat p = filterLogCats (Predicate $ not . any p.getPredicate)
{-# INLINE excludeLogCat #-}

severityThat :: Predicate LogSeverity -> Predicate LogCat
severityThat = contramap (fromMaybe 0 . someSeverity)
{-# INLINE severityThat #-}

noSeverity :: Predicate LogCat
noSeverity = Predicate (isNothing . someSeverity)
{-# INLINE noSeverity #-}

-- | use type applications to check if a log type is present
isLogCat :: forall sub. IsLogCat sub => Predicate LogCat
isLogCat = Predicate $ \(LogCat (_ :: sub')) -> case eqT @sub @sub' of
  Just Refl -> True
  Nothing   -> False
{-# INLINE isLogCat #-}

isLogSubType :: forall sub. IsLogCat sub => Predicate sub -> Predicate LogCat
isLogSubType p = Predicate $ \(LogCat (subType :: sub')) -> case eqT @sub @sub' of
  Just Refl -> p.getPredicate subType
  Nothing   -> False
{-# INLINE isLogSubType #-}

isLogCatName :: ML.ToLogStr n => n -> Predicate LogCat
isLogCatName name = Predicate $ \logCat -> someLogCatName logCat == ML.toLogStr name
{-# INLINE isLogCatName #-}

-- | The Logging module type, a module `Logging a` provides logging capabilities for logs of type `a`
type Logging :: Type -> Type
data Logging a

type LoggingModule = Logging LogData

instance Module (Logging (a :: Type)) where
  newtype ModuleRead  (Logging a) = LoggingRead
    { logging        :: Logger IO a
    }
  data    ModuleState (Logging a) = LoggingState

runLogging
  :: (ConsFDataList c (LoggingModule : mods), Monad m) => Logger IO LogData
  -> EffT' c (LoggingModule : mods) es m a
  -> EffT' c mods es m a
runLogging logger = runEffTOuter_ (LoggingRead logger) LoggingState
{-# INLINE runLogging #-}

instance SystemModule (Logging a) where
  data    ModuleInitData (Logging a) = LoggerInitData
    { loggerInitLogger   :: Logger IO a
    , loggerInitSeverity :: Maybe LogSeverity
    , loggerInitCleanup  :: Maybe (IO ())
    }
  data    ModuleEvent    (Logging a) = LoggingEvent

instance Loadable c (Logging a) mods ies where
  withModule (LoggerInitData logger mSev Nothing) act = case mSev of
    Nothing -> runEffTOuter_ (LoggingRead logger) LoggingState act
    Just s -> runEffTOuter_ (LoggingRead $ anyLogCat (severityThat $ Predicate (>= s)) logger) LoggingState act
  withModule (LoggerInitData logger mSev (Just clean)) act = bracketEffT (return ()) (\_ -> liftIO clean) (\_ -> case mSev of
      Nothing -> runEffTOuter_ (LoggingRead logger) LoggingState act
      Just s -> runEffTOuter_ (LoggingRead $ anyLogCat (severityThat $ Predicate (>= s)) logger) LoggingState act
    )
  {-# INLINE withModule #-}

instance EventLoop c (Logging a) mods es

-- | Maps 'Debug', 'Info', 'Warn', 'Error' to 1, 2, 3, 4 respectively
-- and also accepts numbers between 0 and 10 with a precision of 1 decimal place
defaultStringToLogSeverity :: String -> Either Text LogSeverity
defaultStringToLogSeverity = \case
  "Debug" -> Right 1
  "Info"  -> Right 2
  "Warn"  -> Right 3
  "Error" -> Right 4
  other   -> maybe (Left "Invalid LogLevel, must be one of 'Debug', 'Info', 'Warn', 'Error', or a number between 0 and 10 with a precision of 1 decimal place") Right
    $ readMaybe other
{-# INLINABLE defaultStringToLogSeverity #-}

-- | Load a log level from environment variable LOG_LEVEL,
defaultLoggingFromEnv :: Logger IO LogData -> Maybe (IO ()) -> IO (ModuleInitData LoggingModule)
defaultLoggingFromEnv logger mcl = do
  mLogLevel <- liftIO $ (readMaybe =<<) <$> lookupEnv "LOG_LEVEL"
  return $ LoggerInitData logger mLogLevel mcl
{-# INLINABLE defaultLoggingFromEnv #-}

-- | Load an argument --log-level <level> from command line arguments,
-- if none is provided, it will log everything (Maybe LogSeverity = Nothing)
defaultLoggingFromArgs :: Logger IO LogData -> Maybe (IO ()) -> [String] -> Either Text (ModuleInitData LoggingModule)
defaultLoggingFromArgs logger mcl []         = Right $ LoggerInitData logger Nothing mcl
defaultLoggingFromArgs logger mcl args@(_:_) = do
  level    <- maybe (Right Nothing) (fmap Just) $ detectFlag "--log-level" defaultStringToLogSeverity args
  types    <- sequence $ detectAllFlags "--log-type"    (\case "" -> Left "Empty log type"; s -> Right s) args
  nonTypes <- sequence $ detectAllFlags "--no-log-type" (\case "" -> Left "Empty log type"; s -> Right s) args
  let logger' = foldr ($) logger (  [ anyLogCat     (isLogCatName name) | name <- types ]
                                 <> [ excludeLogCat (isLogCatName name) | name <- nonTypes ]
                                 )
  return $ LoggerInitData logger' level mcl
{-# INLINABLE defaultLoggingFromArgs #-}

monadLoggerAdapter :: Logger IO LogData -> ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()
monadLoggerAdapter logger loc src lev msg = _runLogger logger Log
  { _logType = [mlLogLevelToLogCat lev]
  , _logContent = LogData
      { _logLoc    = Just loc
      , _logSource = Just src
      , _logMsg    = msg
      }
  }
{-# INLINE monadLoggerAdapter #-}

-- | This provides an interface to MonadLogger
instance (MonadIO m, In' c LoggingModule mods) => ML.MonadLogger (EffT' c mods es m) where
  monadLoggerLog loc logsource loglevel msg = do
    LoggingRead logger <- queryModule @LoggingModule
    liftIO $ monadLoggerAdapter logger loc logsource loglevel (ML.toLogStr msg)
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, In' c LoggingModule mods) => ML.MonadLoggerIO (EffT' c mods es m) where
  askLoggerIO = queriesModule @LoggingModule $ (\f loc src lev str -> f
    $ Log [mlLogLevelToLogCat lev]
    $ LogData
      { _logLoc    = Just loc
      , _logSource = Just src
      , _logMsg    = str
      }
    ) . _runLogger . logging
  {-# INLINE askLoggerIO #-}

-- | A compatibility function that works for the old MonadLogger instances
mlLogLevelToLogCat :: ML.LogLevel -> LogCat
mlLogLevelToLogCat ML.LevelDebug     = LogCat Debug
mlLogLevelToLogCat ML.LevelInfo      = LogCat Info
mlLogLevelToLogCat ML.LevelWarn      = LogCat Warn
mlLogLevelToLogCat ML.LevelError     = LogCat Error
mlLogLevelToLogCat (ML.LevelOther t) = LogCat (Other t)
{-# INLINE mlLogLevelToLogCat #-}
