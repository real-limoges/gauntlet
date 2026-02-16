-- |
-- Module      : Log
-- Description : Structured logging with timestamps and log levels
-- Stability   : experimental
--
-- Provides structured logging with timestamps, log levels, and configurable verbosity.
-- Uses co-log-core for efficient logging infrastructure.
module Log
  ( Logger,
    makeLogger,
    logDebug,
    logInfo,
    logWarning,
    logError,
    withTimestamp,
  )
where

import Benchmark.Types (LogLevel (..))
import Colog.Core (LogAction (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (stderr, stdout)

-- | Logger type that carries configuration
data Logger = Logger
  { logLevel :: LogLevel,
    logAction :: LogAction IO (LogLevel, UTCTime, Text)
  }

-- | Format a log message with timestamp and level
formatMessage :: LogLevel -> UTCTime -> Text -> Text
formatMessage level time msg =
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
      levelStr = case level of
        Debug -> "[DEBUG]"
        Info -> "[INFO] "
        Warning -> "[WARN] "
        Error -> "[ERROR]"
   in timestamp <> " " <> levelStr <> " " <> msg

-- | Create a logger with the specified minimum log level
makeLogger :: LogLevel -> Logger
makeLogger minLevel =
  Logger
    { logLevel = minLevel,
      logAction =
        LogAction $ \(level, time, msg) ->
          when (level >= minLevel) $ do
            let formatted = formatMessage level time msg
                handle = if level >= Warning then stderr else stdout
            TIO.hPutStrLn handle formatted
    }
  where
    when True action = action
    when False _ = pure ()

-- | Add timestamp to a log message
withTimestamp :: LogLevel -> Text -> IO (LogLevel, UTCTime, Text)
withTimestamp level msg = do
  time <- getCurrentTime
  return (level, time, msg)

-- | Log at debug level
logDebug :: Logger -> Text -> IO ()
logDebug logger msg = do
  entry <- withTimestamp Debug msg
  unLogAction (logAction logger) entry

-- | Log at info level
logInfo :: Logger -> Text -> IO ()
logInfo logger msg = do
  entry <- withTimestamp Info msg
  unLogAction (logAction logger) entry

-- | Log at warning level
logWarning :: Logger -> Text -> IO ()
logWarning logger msg = do
  entry <- withTimestamp Warning msg
  unLogAction (logAction logger) entry

-- | Log at error level
logError :: Logger -> Text -> IO ()
logError logger msg = do
  entry <- withTimestamp Error msg
  unLogAction (logAction logger) entry
