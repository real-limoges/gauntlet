module Runner.Loop (benchmarkEndpoints) where

import Benchmark.Execution.RateLimiter (makeLimiter)
import Benchmark.Execution.Validation (validateResponses)
import Benchmark.Network
  ( addAuth
  , runBenchmark
  , runBenchmarkDuration
  )
import Benchmark.Report.Output (writeLatenciesWithTarget)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types
  ( Endpoint (..)
  , LoadMode (..)
  , PerfTestError (..)
  , Settings (..)
  , TestingResponse
  , ValidationSummary
  , isDurationBased
  , loadModeDurationSecs
  , totalRequestsForMode
  )
import Control.Concurrent (newQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (logInfo)
import Runner.Context (RunContext (..), emitEvent)
import Runner.Warmup (runWarmup)

-- | Warm up then run the benchmark loop for a labelled set of endpoints.
benchmarkEndpoints :: RunContext -> Text -> [Endpoint] -> IO ([TestingResponse], [ValidationSummary])
benchmarkEndpoints ctx label eps = case eps of
  [] -> throwIO $ NoEndpointsError label
  (firstEp : _) -> do
    runWarmup ctx firstEp
    runEndpointLoop ctx eps

-- | Concurrently benchmark all endpoints and collect responses and validation summaries.
runEndpointLoop :: RunContext -> [Endpoint] -> IO ([TestingResponse], [ValidationSummary])
runEndpointLoop RunContext {..} endpoints = do
  let mode = fromMaybe LoadUnthrottled (loadMode rcSettings)
      iters = totalRequestsForMode mode (iterations rcSettings)
      conc = concurrency rcSettings
  sem <- newQSem conc
  mLimiter <- makeLimiter mode

  logInfo rcLogger $
    T.pack $
      "Starting "
        ++ show (length endpoints)
        ++ " endpoints with "
        ++ show conc
        ++ " concurrency"
        ++ loadModeLabel mode
        ++ "..."

  let indexedEndpoints = zip [1 ..] endpoints
      numEndpoints = length endpoints

  results <-
    mapConcurrently
      ( \(idx, ep) -> do
          emitEvent rcEventChan (EndpointStarted (url ep) idx numEndpoints)
          let authorizedEp = addAuth rcToken ep
          responses <-
            if isDurationBased mode
              then case mLimiter of
                Just limiter ->
                  let dur = realToFrac (loadModeDurationSecs mode)
                   in runBenchmarkDuration rcSettings sem rcManager dur idx authorizedEp limiter rcEventChan
                Nothing -> runBenchmark rcSettings sem rcManager iters idx authorizedEp rcEventChan Nothing
              else runBenchmark rcSettings sem rcManager iters idx authorizedEp rcEventChan mLimiter
          return (idx, ep, responses)
      )
      indexedEndpoints

  writeLatenciesWithTarget rcCsvFile rcTargetName results

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

-- | Human-readable label for log message.
loadModeLabel :: LoadMode -> String
loadModeLabel LoadUnthrottled = ""
loadModeLabel (LoadPoissonRps rps) = " at " ++ show (round rps :: Int) ++ " RPS on average"
loadModeLabel (LoadConstantRps rps) = " at " ++ show (round rps :: Int) ++ " RPS"
loadModeLabel (LoadRampUp s e d) =
  " ramping "
    ++ show (round s :: Int)
    ++ "\8594"
    ++ show (round e :: Int)
    ++ " RPS over "
    ++ show (round d :: Int)
    ++ "s"
loadModeLabel (LoadStepLoad steps) =
  " with " ++ show (length steps) ++ " load steps"
