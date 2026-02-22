-- |
-- Module      : Runner.Context
-- Description : Per-run context record, initialisation, and shared utilities
module Runner.Context
  ( RunContext (..),
    initContext,
    getNowNs,
    emitEvent,
    setupOrFail,
  )
where

import Benchmark.Environment (setupEnvironment)
import Benchmark.Network (NetworkHandle, initNetwork, initNetworkHandle, readToken)
import Benchmark.TUI.State (BenchmarkEvent)
import Benchmark.Types (PerfTestError, Settings (..), exitWithError)
import Benchmark.Types qualified as PT
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, makeLogger)
import Network.HTTP.Client (Manager)
import System.Clock (Clock (Realtime), getTime, toNanoSecs)
import Tracing.Types qualified as TT

data RunContext = RunContext
  { rcSettings :: Settings,
    -- | HTTP manager used for Tempo trace fetching (always HTTP/1.1)
    rcManager :: Manager,
    -- | Network transport for benchmark requests (H1 or H2)
    rcNetwork :: NetworkHandle,
    rcToken :: Text,
    rcCsvFile :: FilePath,
    rcTimestamp :: String,
    rcEventChan :: Maybe (TChan BenchmarkEvent),
    rcLogger :: Logger
  }

-- | Set up the git environment or exit with an error.
setupOrFail :: Text -> Text -> IO ()
setupOrFail branch target =
  setupEnvironment branch target >>= either exitWithError return

-- | Initialise a 'RunContext' from benchmark settings.
initContext :: Settings -> FilePath -> String -> Maybe (TChan BenchmarkEvent) -> Text -> IO RunContext
initContext setts csvFile timestamp eventChan targetUrl = do
  token <- readToken (T.unpack $ secrets setts) >>= either exitWithError return
  mgr <- initNetwork setts
  network <- initNetworkHandle setts targetUrl
  let logger = makeLogger (fromMaybe PT.defaultLogLevel (PT.logLevel setts))
  return
    RunContext
      { rcSettings = setts,
        rcManager = mgr,
        rcNetwork = network,
        rcToken = token,
        rcCsvFile = csvFile,
        rcTimestamp = timestamp,
        rcEventChan = eventChan,
        rcLogger = logger
      }

-- | Current time as nanoseconds (wall clock).
getNowNs :: IO TT.Nanoseconds
getNowNs = fromIntegral . toNanoSecs <$> getTime Realtime

-- | Emit an event to the TUI channel when one is present.
emitEvent :: Maybe (TChan BenchmarkEvent) -> BenchmarkEvent -> IO ()
emitEvent Nothing _ = return ()
emitEvent (Just chan) event = atomically $ writeTChan chan event
