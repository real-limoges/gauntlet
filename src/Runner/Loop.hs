{-|
Module      : Runner.Loop
Description : Endpoint benchmark loop with concurrency and validation
-}
module Runner.Loop (benchmarkEndpoints, runEndpointLoop) where

import Benchmark.Network (addAuth, runBenchmark, runBenchmarkWithEvents)
import Benchmark.Output (writeLatencies)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types
  ( Endpoint (..)
  , PerfTestError (..)
  , Settings (..)
  , TestingResponse
  , ValidationSummary
  , exitWithError
  )
import Benchmark.Validation (validateResponses)
import Control.Concurrent (newQSem)
import Control.Concurrent.Async (mapConcurrently)
import Data.Text qualified as T
import Log (logInfo)
import Runner.Context (RunContext (..), emitEvent)
import Runner.Warmup (runWarmup)

-- | Warm up then run the benchmark loop for a labelled set of endpoints.
benchmarkEndpoints :: RunContext -> String -> [Endpoint] -> IO ([TestingResponse], [ValidationSummary])
benchmarkEndpoints ctx label eps = case eps of
  [] -> exitWithError $ NoEndpointsError label
  (firstEp : _) -> do
    runWarmup ctx firstEp
    runEndpointLoop ctx eps

-- | Concurrently benchmark all endpoints and collect responses and validation summaries.
runEndpointLoop :: RunContext -> [Endpoint] -> IO ([TestingResponse], [ValidationSummary])
runEndpointLoop RunContext {..} endpoints = do
  let iters = iterations rcSettings
      conc = concurrency rcSettings
  sem <- newQSem conc

  logInfo rcLogger $
    T.pack $
      "Starting " ++ show (length endpoints) ++ " endpoints with " ++ show conc ++ " concurrency in parallel..."

  let indexedEndpoints = zip [1 ..] endpoints
      numEndpoints = length endpoints

  results <-
    mapConcurrently
      ( \(idx, ep) -> do
          emitEvent rcEventChan (EndpointStarted (url ep) idx numEndpoints)
          let authorizedEp = addAuth rcToken ep
          responses <- case rcEventChan of
            Just chan -> runBenchmarkWithEvents rcSettings sem rcManager iters idx authorizedEp chan
            Nothing -> runBenchmark rcSettings sem rcManager iters idx authorizedEp
          return (idx, ep, responses)
      )
      indexedEndpoints

  writeLatencies rcCsvFile results

  let allResponses = concatMap (\(_, _, rs) -> rs) results
      summaries =
        concatMap
          ( \(_, ep, rs) ->
              case validate ep of
                Nothing -> []
                Just spec -> [validateResponses spec rs]
          )
          results

  return (allResponses, summaries)
