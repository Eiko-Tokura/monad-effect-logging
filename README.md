# Flexible Logging with Monad-Effect

`monad-effect-logging` is a pure structured logging library for the `monad-effect` ecosystem.

The current API is centered on one unified message payload `LogDoc`.

## Highlights

* You build one `LogDoc` value and decide at the boundary how to render it:

  - plain text
  - ANSI colored text
  - different `Show` strategies
  - custom logger pipelines
  
* Color stays semantic until rendering time. A file logger can ignore color while a console logger can emit ANSI codes from the exact same log event.

* Excellent `TraceId` support

* Open log categories, extensible

* Easy JSON logging by tagging `loggerJson`

This enables you to easily add JSON logging to your files while keeping the console output colorful and readable: you can write no-color json logging to a file while use `pSho` and color constructors to display stuff on the screen at the same time.

## Core types

- `LogEvent` for the event envelope
- `LogWithSourceMeta` for source-location metadata
- `LogDoc` for the structured log message
- `Logger` for the sink
- `LogEffect` for the installed effect

```haskell
data LogEvent a = LogEvent
  { logEventCats :: [LogCat]
  , logEventPayload :: a
  }

data LogWithSourceMeta a = LogWithSourceMeta
  { logMetaLoc :: Maybe Loc
  , logMetaSource :: Maybe LogSource
  , logMetaDoc :: a
  }

newtype Logger m a = Logger
  { runLogger :: LogEvent a -> m ()
  }

data LogEffect m a

data LogDoc
```

The default installed logging effect is:

```haskell
type Logging = LogEffect IO LogDoc
```

## Building messages

String literals work through `IsString`, and deferred values use `logShow`.

```haskell
import Module.Logging

example :: (Monad m, In (LogEffect m LogDoc) mods) => EffT mods es m ()
example = do
  $(logTH Info) $ "starting request " <> logShow (42 :: Int)
  $(logTH Warn) $ logFg Yellow "slow query: " <> logShow ("SELECT ..." :: String)
```

Available smart constructors include:

- `logRaw`
- `logShow`
- `logFg`
- `logBg`
- `logBold`

## Rendering and base loggers

Most applications should use one options-based helper from `Module.Logging.Logger`:

```haskell
import Module.Logging
import Module.Logging.Logger

runApp :: EffT '[LogEffect IO LogDoc] NoError IO () -> IO ()
runApp app = do
  stdoutBase <- createSimpleConcurrentStdoutBaseLogger
  fileBase <- createFileLogger "app.log"

  let stdoutLogger = makeLoggerFromBase ( buildLoggerStyle loggerUseAnsi                ) stdoutBase
  let fileLogger   = makeLoggerFromBase ( buildLoggerStyle (loggerJson . loggerNoStyle) ) fileBase

  runEffT00 $ withLoggerCleanup (stdoutLogger <> fileLogger) app
```

Here each style is a builder function `LoggerOptions -> LoggerOptions`, and the `buildLoggerStyle` function is just a composition of them on the `defaultLoggerStyle`.

For custom pipelines, use the lower-level building blocks:

- `renderLogEvent`
- `loggerFromRenderer`
- your own `Logger`

Styles compose as normal functions:

```haskell
verboseConsole :: LoggerOptions
verboseConsole =
  buildLoggerStyle
    $ loggerUseAnsi
    . loggerOrder [LogTimeChunk, LogCatChunk, LogLocChunk, LogDocChunk]
```

## Categories

Categories are open and extensible:

```haskell
data ProxyLog = Bytes | Logic deriving (Lift)
-- The `Lift` class is only necessary if you want to use them inside `logTH` template haskell
-- utilities, otherwise you can remove it.

instance IsLogCat ProxyLog where
  severity Bytes = severity Debug
  severity Logic = severity Info
  logTypeName Bytes = "BYTES"
  logTypeName Logic = "LOGIC"
```

You can add local categories with `effAddLogCat`, and filter them with the existing combinators.

## `MonadLogger` compatibility

`MonadLogger` and `MonadLoggerIO` are implemented for `LogEffect m LogDoc`.

Incoming `monad-logger` messages are wrapped as `logRaw`, so compatibility fits directly into the unified payload model.

## `TraceId`

This library also provides a super convenient `TraceId` mechanism that can attach scoped trace IDs to log events.
`TraceId` is just a log category. Use `withTraceId` or one of the provided generators from `Module.Logging.TraceId`.

## Status

This release presents the library around `LogDoc`, options-based logger construction, open categories, and boundary-driven rendering.
