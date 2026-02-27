{-|
Module      : Log
Description : Structured logging with timestamps and log levels
Stability   : experimental

Provides structured logging with timestamps, log levels, and configurable verbosity.
-}
module Log
  ( Logger (..)
  , makeLogger
  , logAt
  , logDebug
  , logInfo
  , logWarning
  , logError
  , withTimestamp
  , formatMessage
  )
where

import Benchmark.Types (LogLevel (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (stderr)

-- | Logger type that carries configuration
data Logger = Logger
  { logLevel :: LogLevel
  , logAction :: (LogLevel, UTCTime, Text) -> IO ()
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
    { logLevel = minLevel
    , logAction = \(level, time, msg) ->
        when (level >= minLevel) $ do
          let formatted = "     " <> formatMessage level time msg
          TIO.hPutStrLn stderr formatted
    }
  where
    when True action = action
    when False _ = pure ()

-- | Add timestamp to a log message
withTimestamp :: LogLevel -> Text -> IO (LogLevel, UTCTime, Text)
withTimestamp level msg = do
  time <- getCurrentTime
  return (level, time, msg)

-- | Log at a given level
logAt :: LogLevel -> Logger -> Text -> IO ()
logAt level logger msg = do
  entry <- withTimestamp level msg
  logAction logger entry

logDebug, logInfo, logWarning, logError :: Logger -> Text -> IO ()
logDebug = logAt Debug
logInfo = logAt Info
logWarning = logAt Warning
logError = logAt Error
