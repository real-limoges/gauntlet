-- | Warmup request execution before benchmark runs.
module Runner.Warmup (runWarmup) where

import Benchmark.Network.Auth (addAuth)
import Benchmark.Network.Exec (runBenchmark)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types (Endpoint, defaultWarmupSettings)
import Benchmark.Types qualified as PT
import Control.Concurrent (newQSem)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Log (logInfo)
import Runner.Context (RunContext (..), emitEvent, makeBenchmarkEnv)

{-| Execute warmup iterations against the first endpoint.
Respects 'WarmupSettings' from 'Settings'; skips if iterations = 0.
-}
runWarmup :: RunContext -> Endpoint -> IO ()
runWarmup ctx@RunContext {..} ep = do
  let warmupSettings = fromMaybe defaultWarmupSettings (PT.warmup rcSettings)
      warmupIters = PT.warmupIterations warmupSettings
  when (warmupIters > 0) $ do
    let label =
          "Warming up ("
            <> T.pack (show warmupIters)
            <> " iteration"
            <> (if warmupIters == 1 then "" else "s")
            <> ")..."
    logInfo rcLogger label
    emitEvent rcEventChan (StatusMessage label)
    sem <- newQSem 1
    let authorizedEp = addAuth rcToken ep
        env = makeBenchmarkEnv ctx sem 1
    void $ runBenchmark env warmupIters authorizedEp Nothing
