{-|
Module      : Runner
Description : Benchmark orchestration
Stability   : experimental

Orchestrates single-target benchmark runs ('runSingle').
Delegates to sub-modules for context setup, warmup, looping, tracing, and baseline handling.
-}
module Runner (runSingle) where

import Benchmark.CLI (BaselineMode)
import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initOutputFiles, writeMarkdownReport)
import Benchmark.Report (printSingleBenchmarkReport, printValidationSummary)
import Benchmark.Report.Markdown (markdownSingleReport, markdownValidationReport)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState, tsFinished)
import Benchmark.Types
  ( OutputFormat (..)
  , PerfTestError (..)
  , RunResult (..)
  , Settings (..)
  , Targets (..)
  , TestConfig (..)
  , exitWithError
  )
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (newTChanIO)
import Control.Exception (onException)
import Data.Text qualified as T
import Lens.Micro ((^.))
import Runner.Baseline (handleBaseline)
import Runner.Context (RunContext (..), emitEvent, getNowNs, initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (calculateStats)

-- | Run single-target benchmark without comparison.
runSingle :: BaselineMode -> OutputFormat -> TestConfig -> IO RunResult
runSingle baselineMode outFmt cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let eps = buildEndpoints (candidate (targets cfg)) (payloads cfg)
      setts = settings cfg
      targetUrl = candidate $ targets cfg

  eventChan <- newTChanIO

  let totalRequests = iterations setts * length eps
      numEndpoints = length eps
      tuiState = initialState targetUrl totalRequests numEndpoints

  ctx <- initContext setts csvFile timestamp (Just eventChan)

  benchmarkWork <- async $
    (`onException` emitEvent (Just eventChan) BenchmarkFinished) $ do
      startNs <- getNowNs
      emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> candidate (git cfg) <> "...")
      setupOrFail
        setts
        (candidate $ git cfg)
        (candidate $ targets cfg)
        (Just ["--profile", "testing", "up", "-d", "--build"])
      emitEvent (Just eventChan) (StatusMessage $ "Running " <> targetUrl <> " (" <> candidate (git cfg) <> ")")
      (results, validSummaries) <- benchmarkEndpoints ctx "endpoints" eps
      endNs <- getNowNs

      emitEvent (Just eventChan) BenchmarkFinished
      return (results, validSummaries, startNs, endNs)

  finalState <- runTUI eventChan tuiState

  if not (finalState ^. tsFinished)
    then do
      cancel benchmarkWork
      exitWithError $ EnvironmentSetupError "Benchmark cancelled by user"
    else do
      (results, validSummaries, startNs, endNs) <- wait benchmarkWork

      let stats = calculateStats results

      printSingleBenchmarkReport targetUrl stats
      printValidationSummary validSummaries

      writeMarkdownReport outFmt $
        markdownSingleReport targetUrl stats
          <> markdownValidationReport validSummaries

      runTraceAnalysis (rcLogger ctx) (rcManager ctx) setts timestamp startNs endNs

      handleBaseline (rcLogger ctx) baselineMode (T.pack timestamp) stats
