-- | Structured logging with configurable verbosity levels.
module Log
  ( Logger (..)
  , makeLogger
  , logAt
  , logDebug
  , logInfo
  , logWarning
  , logError
  , formatMessage
  )
where

import Benchmark.Types (LogLevel (..))
import Control.Monad (when)
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

-- | Log a message at 'Debug' level.
logDebug :: Logger -> Text -> IO ()
logDebug = logAt Debug

-- | Log a message at 'Info' level.
logInfo :: Logger -> Text -> IO ()
logInfo = logAt Info

-- | Log a message at 'Warning' level.
logWarning :: Logger -> Text -> IO ()
logWarning = logAt Warning

-- | Log a message at 'Error' level.
logError :: Logger -> Text -> IO ()
logError = logAt Error

-- | Log at a given level
logAt :: LogLevel -> Logger -> Text -> IO ()
logAt level logger msg = do
  entry <- withTimestamp level msg
  logAction logger entry

-- | Add timestamp to a log message
withTimestamp :: LogLevel -> Text -> IO (LogLevel, UTCTime, Text)
withTimestamp level msg = do
  time <- getCurrentTime
  return (level, time, msg)

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
