-- | Run context initialization and environment setup helpers.
module Runner.Context
  ( RunContext (..)
  , initContext
  , getNowNs
  , emitEvent
  , setupOrFail
  )
where

import Benchmark.Execution.Environment (setupEnvironment)
import Benchmark.Network (initNetwork, readToken)
import Benchmark.TUI.State (BenchmarkEvent)
import Benchmark.Types (Settings (..), exitWithError)
import Benchmark.Types qualified as PT
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, makeLogger)
import Network.HTTP.Client (Manager)
import System.Clock (Clock (Realtime), getTime, toNanoSecs)
import Tracing.Types qualified as TT

-- | Shared state threaded through every benchmark phase.
data RunContext = RunContext
  { rcSettings :: Settings
  -- ^ Benchmark configuration
  , rcManager :: Manager
  -- ^ HTTP manager for all requests (benchmark + Tempo trace fetching)
  , rcToken :: Text
  -- ^ Bearer token for authenticated endpoints (empty when no auth configured)
  , rcCsvFile :: FilePath
  -- ^ Path to the CSV latency output file
  , rcTimestamp :: String
  -- ^ ISO-8601 timestamp string identifying this run
  , rcEventChan :: Maybe (TChan BenchmarkEvent)
  -- ^ Optional TUI event channel for real-time progress
  , rcLogger :: Logger
  -- ^ Logger with configured verbosity level
  , rcTargetName :: Text
  -- ^ Name of the current target being benchmarked
  }

-- | Initialise a 'RunContext' from benchmark settings.
initContext :: Settings -> FilePath -> String -> Maybe (TChan BenchmarkEvent) -> IO RunContext
initContext setts csvFile timestamp eventChan = do
  token <- case secrets setts of
    Nothing -> return T.empty
    Just path -> readToken (T.unpack path) >>= either exitWithError return
  mgr <- initNetwork setts
  let logger = makeLogger (fromMaybe PT.defaultLogLevel (PT.logLevel setts))
  return
    RunContext
      { rcSettings = setts
      , rcManager = mgr
      , rcToken = token
      , rcCsvFile = csvFile
      , rcTimestamp = timestamp
      , rcEventChan = eventChan
      , rcLogger = logger
      , rcTargetName = ""
      }

-- | Set up the git environment or exit with an error.
setupOrFail :: Manager -> Settings -> Text -> Text -> Maybe [String] -> IO ()
setupOrFail mgr setts branch target composeArgs =
  setupEnvironment mgr setts branch target composeArgs >>= either throwIO return

-- | Current time as nanoseconds (wall clock).
getNowNs :: IO TT.Nanoseconds
getNowNs = fromIntegral . toNanoSecs <$> getTime Realtime

-- | Emit an event to the TUI channel when one is present.
emitEvent :: Maybe (TChan BenchmarkEvent) -> BenchmarkEvent -> IO ()
emitEvent Nothing _ = return ()
emitEvent (Just chan) event = atomically $ writeTChan chan event
