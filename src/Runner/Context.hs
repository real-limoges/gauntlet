-- | Run context initialization and environment setup helpers.
module Runner.Context
  ( RunContext (..)
  , initContext
  , emitEvent
  , makeBenchmarkEnv
  )
where

import Benchmark.Network.Auth (readToken)
import Benchmark.Network.Exec (BenchmarkEnv (..))
import Benchmark.Network.Request (initNetwork)
import Benchmark.TUI.State (BenchmarkEvent)
import Benchmark.Types (Settings (..), exitWithError)
import Benchmark.Types qualified as PT
import Control.Concurrent (QSem)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, makeLogger)
import Network.HTTP.Client (Manager)

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

-- | Build a 'BenchmarkEnv' from shared 'RunContext' fields plus loop-local state.
makeBenchmarkEnv :: RunContext -> QSem -> Int -> BenchmarkEnv
makeBenchmarkEnv RunContext {..} sem idx =
  BenchmarkEnv
    { beSettings = rcSettings
    , beSem = sem
    , beManager = rcManager
    , bePayloadIndex = idx
    , beEventChan = rcEventChan
    , beLogger = rcLogger
    }

-- | Emit an event to the TUI channel when one is present.
emitEvent :: Maybe (TChan BenchmarkEvent) -> BenchmarkEvent -> IO ()
emitEvent Nothing _ = return ()
emitEvent (Just chan) event = atomically $ writeTChan chan event
