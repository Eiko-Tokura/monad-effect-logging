# Flexible Logging with Monad-Effect

`monad-effect-logging` is a pure structured logging library for the `monad-effect` ecosystem.

The current API is centered on one unified message payload:

- `LogEvent` for the event envelope
- `LogWithSourceMeta` for source-location metadata
- `LogDoc` for the structured log message
- `Logger` for the sink
- `LogEffect` for the installed effect

## Why this version

Older versions exposed two payload styles, `LogS` and `LogB`. This version removes that split.

You now build one `LogDoc` value and decide at the boundary how to render it:

- plain text
- ANSI colored text
- different `Show` strategies
- custom logger pipelines

Color stays semantic until rendering time. A file logger can ignore color while a console logger can emit ANSI codes from the exact same log event.

## Core types

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
  $(logTH Warn) $ logFg (Named Yellow) "slow query: " <> logShow ("SELECT ..." :: String)
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

  let stdoutLogger =
        makeLoggerFromBase
          defaultLoggerStyle
            { loggerDocRenderOptions =
                defaultDocRenderOptions { docRenderStyleMode = AnsiStyles }
            }
          stdoutBase

  let fileLogger =
        makeLoggerFromBase
          defaultLoggerStyle
            { loggerDocRenderOptions =
                defaultDocRenderOptions { docRenderStyleMode = NoStyles }
            }
          fileBase

  runEffT00 $ withLoggerCleanup (stdoutLogger <> fileLogger) app
```

For custom pipelines, use the lower-level building blocks:

- `renderLogEvent`
- `loggerFromRenderer`
- your own `Logger`

## Categories

Categories are still open and extensible:

```haskell
data ProxyLog = Bytes | Logic deriving (Lift)

instance IsLogCat ProxyLog where
  severity Bytes = severity Debug
  severity Logic = severity Info
  logTypeDisplay Bytes = "BYTES"
  logTypeDisplay Logic = "LOGIC"
```

You can add local categories with `effAddLogCat`, and filter them with the existing combinators.

## `MonadLogger` compatibility

`MonadLogger` and `MonadLoggerIO` are implemented for `LogEffect m LogDoc`.

Incoming `monad-logger` messages are wrapped as `logRaw`, so compatibility does not require a second payload type anymore.

## `TraceId`

`TraceId` is still a category-level concern. Use `withTraceId` or one of the provided generators from `Module.Logging.TraceId`.

## Status

This is a breaking API redesign. Existing code written against `LogS` / `LogB` will need to move to `LogDoc`.
