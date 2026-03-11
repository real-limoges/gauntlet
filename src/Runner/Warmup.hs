module Runner.Warmup (runWarmup) where

import Benchmark.Network (addAuth, runBenchmark)
import Benchmark.Types (Endpoint, defaultWarmupSettings)
import Benchmark.Types qualified as PT
import Control.Concurrent (newQSem)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Log (logInfo)
import Runner.Context (RunContext (..))

{-| Execute warmup iterations against the first endpoint.
Respects 'WarmupSettings' from 'Settings'; skips if iterations = 0.
-}
runWarmup :: RunContext -> Endpoint -> IO ()
runWarmup RunContext {..} ep = do
  let warmupSettings = fromMaybe defaultWarmupSettings (PT.warmup rcSettings)
      warmupIters = PT.warmupIterations warmupSettings
  when (warmupIters > 0) $ do
    logInfo rcLogger $
      "Warming up ("
        <> T.pack (show warmupIters)
        <> " iteration"
        <> (if warmupIters == 1 then "" else "s")
        <> ")..."
    sem <- newQSem 1
    let authorizedEp = addAuth rcToken ep
    void $ runBenchmark rcSettings sem rcManager warmupIters 1 authorizedEp Nothing
