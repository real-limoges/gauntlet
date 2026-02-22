-- |
-- Module      : Runner.Warmup
-- Description : Warmup execution before the main benchmark
module Runner.Warmup (runWarmup) where

import Benchmark.Network (addAuth, runBenchmark)
import Benchmark.Types (Endpoint, Settings (..), defaultWarmupSettings)
import Benchmark.Types qualified as PT
import Control.Concurrent (newQSem)
import Control.Monad (when)
import Data.Text qualified as T
import Log (logInfo)
import Runner.Context (RunContext (..))

-- | Execute warmup iterations against the first endpoint.
-- Respects 'WarmupSettings' from 'Settings'; skips if iterations = 0.
runWarmup :: RunContext -> Endpoint -> IO ()
runWarmup RunContext {..} ep = do
  let warmupSettings = maybe defaultWarmupSettings id (PT.warmup rcSettings)
      warmupIters = PT.warmupIterations warmupSettings
  when (warmupIters > 0) $ do
    logInfo rcLogger $
      T.pack $
        "Warming up ("
          ++ show warmupIters
          ++ " iteration"
          ++ (if warmupIters == 1 then "" else "s")
          ++ ")..."
    sem <- newQSem 1
    let authorizedEp = addAuth rcToken ep
    _ <- runBenchmark rcSettings sem rcNetwork warmupIters 1 authorizedEp
    return ()
