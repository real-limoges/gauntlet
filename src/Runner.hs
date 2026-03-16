-- | Single-endpoint benchmark runner.
module Runner (runSingle) where

import Benchmark.Config.CLI (BaselineMode)
import Benchmark.Config.Loader (buildEndpoints)
import Benchmark.Report.Baseline (handleBaseline)
import Benchmark.Report.Output (initOutputFiles)
import Benchmark.Reporter (Reporter (..), combineReporters)
import Benchmark.Reporter.Plot (plotReporter)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState)
import Benchmark.Types
  ( ChartsSettings
  , RunResult (..)
  , Settings (..)
  , Targets (..)
  , TestConfig (..)
  )
import Control.Concurrent.STM (newTChanIO)
import Control.Monad (unless)
import Data.Text qualified as T
import Runner.Benchmark (withTUI)
import Runner.Context
  ( Branch (..)
  , RunContext (..)
  , ServiceUrl (..)
  , emitEvent
  , getNowNs
  , initContext
  , setupOrFail
  )
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

  (results, validSummaries, startNs, endNs) <- withTUI eventChan tuiState $ do
    startNs <- getNowNs
    let branch = candidate (git cfg)
    unless (T.null branch) $ do
      emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> branch <> "...")
      setupOrFail
        (rcManager ctx)
        setts
        (Branch branch)
        (ServiceUrl $ candidate $ targets cfg)
        (Just ["--profile", "testing", "up", "-d", "--build"])
    emitEvent
      (Just eventChan)
      ( StatusMessage $
          "Running " <> targetUrl <> maybe "" (\b -> " (" <> b <> ")") (if T.null branch then Nothing else Just branch)
      )
    (benchResults, validSummaries) <- benchmarkEndpoints ctx "endpoints" eps
    endNs <- getNowNs
    return (benchResults, validSummaries, startNs, endNs)

  let stats = calculateStats results

  reportSingle fullReporter targetUrl stats validSummaries

  runTraceAnalysis ctx timestamp startNs endNs

  handleBaseline fullReporter (rcLogger ctx) baselineMode (T.pack timestamp) stats
