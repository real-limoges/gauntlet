module Runner (runSingle) where

import Benchmark.Config.CLI (BaselineMode)
import Benchmark.Config.Loader (buildEndpoints)
import Benchmark.Report.Baseline (handleBaseline)
import Benchmark.Report.Output (initOutputFiles)
import Benchmark.Reporter (Reporter (..), combineReporters)
import Benchmark.Reporter.Plot (plotReporter)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState, tsError, tsFinished)
import Benchmark.Types
  ( ChartsSettings
  , PerfTestError (..)
  , RunResult (..)
  , Settings (..)
  , Targets (..)
  , TestConfig (..)
  , exitWithError
  )
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (newTChanIO)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO, try)
import Control.Monad (void)
import Data.Text qualified as T
import Runner.Context (RunContext (..), emitEvent, getNowNs, initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (calculateStats)

-- | Run single-target benchmark without comparison.
runSingle :: Reporter -> BaselineMode -> Maybe ChartsSettings -> TestConfig -> IO RunResult
runSingle reporter baselineMode mCharts cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let fullReporter = case mCharts of
        Nothing -> reporter
        Just cs -> combineReporters [reporter, plotReporter cs csvFile]

  let eps = buildEndpoints (candidate (targets cfg)) (payloads cfg)
      setts = settings cfg
      targetUrl = candidate $ targets cfg

  eventChan <- newTChanIO

  let totalRequests = iterations setts * length eps
      numEndpoints = length eps
      tuiState = initialState targetUrl totalRequests numEndpoints

  baseCtx <- initContext setts csvFile timestamp (Just eventChan)
  let ctx = baseCtx {rcTargetName = targetUrl}

  benchmarkWork <- async $ do
    let work = do
          startNs <- getNowNs
          emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> candidate (git cfg) <> "...")
          setupOrFail
            (rcManager ctx)
            setts
            (candidate $ git cfg)
            (candidate $ targets cfg)
            (Just ["--profile", "testing", "up", "-d", "--build"])
          emitEvent (Just eventChan) (StatusMessage $ "Running " <> targetUrl <> " (" <> candidate (git cfg) <> ")")
          (results, validSummaries) <- benchmarkEndpoints ctx "endpoints" eps
          endNs <- getNowNs
          return (results, validSummaries, startNs, endNs)
    result <- try work
    case result of
      Right val -> do
        emitEvent (Just eventChan) BenchmarkFinished
        return val
      Left ex
        | Just (_ :: SomeAsyncException) <- fromException ex -> throwIO ex
        | otherwise -> do
            emitEvent (Just eventChan) (BenchmarkFailed (T.pack (show ex)))
            throwIO ex

  finalState <- runTUI eventChan tuiState

  case (tsFinished finalState, tsError finalState) of
    (False, _) -> do
      cancel benchmarkWork
      exitWithError BenchmarkCancelled
    (True, Just errMsg) -> do
      void (wait benchmarkWork) `catch` \(_ :: SomeException) -> return ()
      exitWithError (UnknownNetworkError errMsg)
    (True, Nothing) -> do
      (results, validSummaries, startNs, endNs) <- wait benchmarkWork

      let stats = calculateStats results

      reportSingle fullReporter targetUrl stats validSummaries

      runTraceAnalysis (rcLogger ctx) (rcManager ctx) setts timestamp startNs endNs

      handleBaseline fullReporter (rcLogger ctx) baselineMode (T.pack timestamp) stats
