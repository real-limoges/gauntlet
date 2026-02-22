-- |
-- Module      : Runner
-- Description : Benchmark orchestration
-- Stability   : experimental
--
-- Orchestrates A/B benchmark runs ('runMultiple') and single-target runs ('runSingle').
-- Delegates to sub-modules for context setup, warmup, looping, tracing, and baseline handling.
module Runner (runMultiple, runSingle) where

import Benchmark.CLI (BaselineMode)
import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initOutputFiles, resultsDir)
import Benchmark.Plotting (plotDistributions)
import Benchmark.Report (printMultipleBenchmarkReport, printSingleBenchmarkReport, printValidationSummary)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState)
import Benchmark.Types
  ( Nanoseconds (..),
    PerfTestError (..),
    RunResult (..),
    Settings (..),
    TestConfig (..),
    TestingResponse (..),
    Targets (..),
    exitWithError,
  )
import Control.Concurrent.Async (async, wait)
import Control.Exception (onException)
import Data.Text qualified as T
import Control.Concurrent.STM (newTChanIO)
import Runner.Baseline (handleBaseline)
import Runner.Context (RunContext (..), emitEvent, getNowNs, initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (calculateStats, compareBayesian)

-- | Run A/B benchmark comparing candidate against primary target.
runMultiple :: BaselineMode -> TestConfig -> IO RunResult
runMultiple baselineMode cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let epsCandidate = buildEndpoints cfg False
      epsPrimary = buildEndpoints cfg True
      setts = settings cfg

  case (epsCandidate, epsPrimary) of
    ([], _) -> exitWithError $ NoEndpointsError "candidate"
    (_, []) -> exitWithError $ NoEndpointsError "primary"
    _ -> do
      eventChan <- newTChanIO

      let totalRequests = iterations setts * (length epsCandidate + length epsPrimary)
          numEndpoints = length epsCandidate + length epsPrimary
          targetUrl = candidate (targets cfg) <> " vs " <> primary (targets cfg)
          tuiState = initialState targetUrl totalRequests numEndpoints

      ctx <- initContext setts csvFile timestamp (Just eventChan) (candidate (targets cfg))

      benchmarkWork <- async $
        (`onException` emitEvent (Just eventChan) BenchmarkFinished) $ do
          startNs <- getNowNs

          setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
          (resultsCandidate, validCandidate) <- benchmarkEndpoints ctx "candidate" epsCandidate

          setupOrFail (primary $ git cfg) (primary $ targets cfg)
          (resultsPrimary, validPrimary) <- benchmarkEndpoints ctx "primary" epsPrimary

          endNs <- getNowNs

          emitEvent (Just eventChan) BenchmarkFinished
          return (resultsCandidate, validCandidate, resultsPrimary, validPrimary, startNs, endNs)

      _ <- runTUI eventChan tuiState

      (resultsCandidate, validCandidate, resultsPrimary, validPrimary, startNs, endNs) <- wait benchmarkWork

      let statsCandidate = calculateStats resultsCandidate
          statsPrimary = calculateStats resultsPrimary
          bayes = compareBayesian statsPrimary statsCandidate

      printMultipleBenchmarkReport "primary" "candidate" statsPrimary statsCandidate bayes
      printValidationSummary (validPrimary ++ validCandidate)

      let timesPrimary = map (fromIntegral . unNanoseconds . durationNs) resultsPrimary
          timesCandidate = map (fromIntegral . unNanoseconds . durationNs) resultsCandidate
          plotFile = resultsDir ++ "/kde_plot-" ++ timestamp ++ ".png"
      plotDistributions timesPrimary timesCandidate plotFile
      runTraceAnalysis (rcLogger ctx) (rcManager ctx) setts timestamp startNs endNs

      handleBaseline (rcLogger ctx) baselineMode (T.pack timestamp) statsCandidate

-- | Run single-target benchmark without comparison.
runSingle :: BaselineMode -> TestConfig -> IO RunResult
runSingle baselineMode cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let eps = buildEndpoints cfg False
      setts = settings cfg
      targetUrl = candidate $ targets cfg

  eventChan <- newTChanIO

  let totalRequests = iterations setts * length eps
      numEndpoints = length eps
      tuiState = initialState targetUrl totalRequests numEndpoints

  ctx <- initContext setts csvFile timestamp (Just eventChan) targetUrl

  benchmarkWork <- async $
    (`onException` emitEvent (Just eventChan) BenchmarkFinished) $ do
      startNs <- getNowNs
      setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
      (results, validSummaries) <- benchmarkEndpoints ctx "endpoints" eps
      endNs <- getNowNs

      emitEvent (Just eventChan) BenchmarkFinished
      return (results, validSummaries, startNs, endNs)

  _ <- runTUI eventChan tuiState

  (results, validSummaries, startNs, endNs) <- wait benchmarkWork

  let stats = calculateStats results

  printSingleBenchmarkReport targetUrl stats
  printValidationSummary validSummaries
  runTraceAnalysis (rcLogger ctx) (rcManager ctx) setts timestamp startNs endNs

  handleBaseline (rcLogger ctx) baselineMode (T.pack timestamp) stats
