{-# LANGUAGE TemplateHaskell, UndecidableInstances, AllowAmbiguousTypes, DeriveLift, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | We want a logger that supports open categories, open levels, open severities etc.
-- so that we can filter on different levels for different categories
--
-- finally we will also provide an interface for MonadLogger for compatibility
module Module.Logging
  ( -- * Definitions
    LogSeverity
  , IsLogCat(..)
  , LogCat(..)
  , Log(..), logType, logContent
  , LogMsg(..), logLoc, logSource, logMsg
  , Logger(..), runLogger
  , Logging
  , LoggingModule
  -- * Default log categories
  , Debug(..)
  , Info(..)
  , Warn(..)
  , Error(..)
  , Other(..)
  -- * Default log implementation
  , toLogStrS
  , logLog
  , LogS
  , LogData
  -- * Combinators
  , localLogger
  , localLog
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
  , someSeverity
  , someLogCatName
  -- * Running and initializing
  , runLogging
  , withLiftLogger
  -- * Other optional utilities
  , defaultStringToLogSeverity
  , defaultLoggingFromEnv
  , defaultLoggingFromArgs
  , monadLoggerAdapter
  , mlLogLevelToLogCat

  -- * Re-export
  , ModuleRead(..)
  , ModuleState(..)
  , ModuleInitData(..)
  , ModuleEvent(..)
  -- * Re-export contravariant functors
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

import qualified Language.Haskell.TH.Syntax as TH

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

type LogS = LogMsg ML.LogStr

type LogData = LogS
{-# DEPRECATED LogData "Use LogS instead" #-}

data LogMsg a = LogMsg
  { _logLoc    :: Maybe ML.Loc
  , _logSource :: Maybe ML.LogSource
  , _logMsg    :: a
  }

makeLenses ''Log
makeLenses ''LogMsg
makeLenses ''Loc

instance Functor LogMsg where
  fmap f logMsg' = logMsg' { _logMsg = f (_logMsg logMsg') }
  {-# INLINE fmap #-}

instance Semigroup a => Semigroup (LogMsg a) where
  l1 <> l2 = LogMsg
    { _logLoc = l1 ^. logLoc <|> l2 ^. logLoc
    , _logSource = l1 ^. logSource <|> l2 ^. logSource
    , _logMsg = l1 ^. logMsg <> l2 ^. logMsg
    }
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (LogMsg a) where
  mempty = LogMsg Nothing Nothing mempty
  {-# INLINE mempty #-}

-- | Some default log types, you can easily define your own
data    Debug = Debug  deriving TH.Lift
data    Info  = Info   deriving TH.Lift
data    Warn  = Warn   deriving TH.Lift
data    Error = Error  deriving TH.Lift
newtype Other = Other Text  deriving TH.Lift

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

liftLogger :: (m () -> n ()) -> Logger m b -> Logger n b
liftLogger f (Logger g) = Logger (f . g)
{-# INLINE liftLogger #-}

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
localLogger :: forall a c m mods es b. (Monad m, In' c (Logging m a) mods) => (Logger m a -> Logger m a) -> EffT' c mods es m b -> EffT' c mods es m b
localLogger f = localModule (\(LoggingRead logger) -> LoggingRead (f logger))
{-# INLINE localLogger #-}

-- | Locally modify the log
localLog :: forall a m mods es c. (Monad m, Logging m a `In` mods) => (Log a -> Log a) -> EffT mods es m c -> EffT mods es m c
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
-- effAddLogCat @LogS (LogCat ConnectionPool) $ do
--   ...
-- @
effAddLogCat :: forall a c mods es m b. (Monad m, In' c (Logging m a) mods) => LogCat -> EffT' c mods es m b -> EffT' c mods es m b
effAddLogCat logCat = localLogger @a (addLogCat logCat)
{-# INLINE effAddLogCat #-}

-- | Add a log category to the log in EffT (defaulting to In (Logging m LogS) mods)
-- @
-- effAddLogCat' (LogCat ConnectionPool) $ do
--   ...
-- @
effAddLogCat' :: forall c mods es m b. (Monad m, In' c (Logging m LogS) mods) => LogCat -> EffT' c mods es m b -> EffT' c mods es m b
effAddLogCat' logCat = localLogger @LogS (addLogCat logCat)
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

-- | The Logging m module type, a module `Logging m a` provides logging capabilities for logs of type `a`
type Logging :: (Type -> Type) -> Type -> Type
data Logging m a

type LoggingModule = Logging IO LogS -- standard logging module

withLiftLogger
  :: forall m n c a mods es b.
  ( Monad m
  , In' c (Logging m a) (Logging m a : mods)
  , ConsFDataList c (Logging n a : mods)
  , ConsFDataList c (Logging m a : mods)
  )
  => (forall x. m x -> n x) -> EffT' c (Logging n a : mods) es m b -> EffT' c (Logging m a : mods) es m b
withLiftLogger lifter act = do
  LoggingRead logger <- askModule @(Logging m a)
  let logger' = liftLogger lifter logger
  embedMods $ runEffTOuter_ (LoggingRead logger') LoggingState act

instance Module (Logging m (a :: Type)) where
  newtype ModuleRead  (Logging m a) = LoggingRead
    { logging :: Logger m a
    }
  data    ModuleState (Logging m a) = LoggingState

runLogging
  :: (ConsFDataList c (Logging m logS : mods), Monad m) => Logger m logS
  -> EffT' c (Logging m logS : mods) es m a
  -> EffT' c mods es m a
runLogging logger = runEffTOuter_ (LoggingRead logger) LoggingState
{-# INLINE runLogging #-}

instance SystemModule (Logging m a) where
  data    ModuleInitData (Logging m a) = LoggingInitData
    { loggerInitLogger   :: Logger IO a
    , loggerInitSeverity :: Maybe LogSeverity
    , loggerInitCleanup  :: Maybe (IO ())
    }
  data    ModuleEvent    (Logging m a) = LoggingEvent

instance Loadable c (Logging IO a) mods ies where
  withModule (LoggingInitData logger mSev Nothing) act = case mSev of
    Nothing -> runEffTOuter_ (LoggingRead logger) LoggingState act
    Just s -> runEffTOuter_ (LoggingRead $ anyLogCat (severityThat $ Predicate (>= s)) logger) LoggingState act
  withModule (LoggingInitData logger mSev (Just clean)) act = bracketEffT (return ()) (\_ -> liftIO clean) (\_ -> case mSev of
      Nothing -> runEffTOuter_ (LoggingRead logger) LoggingState act
      Just s -> runEffTOuter_ (LoggingRead $ anyLogCat (severityThat $ Predicate (>= s)) logger) LoggingState act
    )
  {-# INLINE withModule #-}

instance EventLoop c (Logging m a) mods es

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
defaultLoggingFromEnv :: Logger IO LogS -> Maybe (IO ()) -> IO (ModuleInitData LoggingModule)
defaultLoggingFromEnv logger mcl = do
  mLogLevel <- liftIO $ (readMaybe =<<) <$> lookupEnv "LOG_LEVEL"
  return $ LoggingInitData logger mLogLevel mcl
{-# INLINABLE defaultLoggingFromEnv #-}

-- | Load an argument --log-level <level> from command line arguments,
-- if none is provided, it will log everything (Maybe LogSeverity = Nothing)
defaultLoggingFromArgs :: Logger IO LogS -> Maybe (IO ()) -> [String] -> Either Text (ModuleInitData LoggingModule)
defaultLoggingFromArgs logger mcl []         = Right $ LoggingInitData logger Nothing mcl
defaultLoggingFromArgs logger mcl args@(_:_) = do
  level    <- maybe (Right Nothing) (fmap Just) $ detectFlag "--log-level" defaultStringToLogSeverity args
  types    <- sequence $ detectAllFlags "--log-type"    (\case "" -> Left "Empty log type"; s -> Right s) args
  nonTypes <- sequence $ detectAllFlags "--no-log-type" (\case "" -> Left "Empty log type"; s -> Right s) args
  let logger' = foldr ($) logger (  [ anyLogCat     (isLogCatName name) | name <- types ]
                                 <> [ excludeLogCat (isLogCatName name) | name <- nonTypes ]
                                 )
  return $ LoggingInitData logger' level mcl
{-# INLINABLE defaultLoggingFromArgs #-}

monadLoggerAdapter :: Logger m LogS -> ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> m ()
monadLoggerAdapter logger loc src lev msg = _runLogger logger Log
  { _logType = [mlLogLevelToLogCat lev]
  , _logContent = LogMsg
      { _logLoc    = Just loc
      , _logSource = Just src
      , _logMsg    = msg
      }
  }
{-# INLINE monadLoggerAdapter #-}

-- | This provides an interface to MonadLogger
instance (Monad m, In' c (Logging m LogS) mods) => ML.MonadLogger (EffT' c mods es m) where
  monadLoggerLog loc logsource loglevel msg = do
    LoggingRead logger <- queryModule @(Logging m LogS)
    lift $ monadLoggerAdapter logger loc logsource loglevel (ML.toLogStr msg)
  {-# INLINE monadLoggerLog #-}

instance (m ~ IO, In' c (Logging m LogS) mods) => ML.MonadLoggerIO (EffT' c mods es m) where
  askLoggerIO = queriesModule @(Logging m LogS) $ (\f loc src lev str -> f
    $ Log [mlLogLevelToLogCat lev]
    $ LogMsg
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

logLog :: forall a c mods es m. (Monad m, In' c (Logging m a) mods) => Log a -> EffT' c mods es m ()
logLog logd = asksModule @(Logging m a) (_runLogger . logging) >>= (\action -> lift $ action logd)
{-# INLINABLE logLog #-}

-- | Apply 'show' to convert a value to LogStr
toLogStrS :: Show a => a -> ML.LogStr
toLogStrS = ML.toLogStr . show
{-# INLINE toLogStrS #-}
