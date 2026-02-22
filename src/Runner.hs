{- |
Module      : Runner
Description : Benchmark orchestration
Stability   : experimental

Orchestrates A/B benchmark runs ('runMultiple') and single-target runs ('runSingle').
Delegates to sub-modules for context setup, warmup, looping, tracing, and baseline handling.
-}
module Runner (runMultiple, runSingle) where

import Benchmark.CLI (BaselineMode)
import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initOutputFiles, resultsDir)
import Benchmark.Plotting (plotDistributions)
import Benchmark.Report (printMultipleBenchmarkReport, printSingleBenchmarkReport, printValidationSummary)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState, tsFinished)
import Benchmark.Types (
    Nanoseconds (..),
    PerfTestError (..),
    RunResult (..),
    Settings (..),
    Targets (..),
    TestConfig (..),
    TestingResponse (..),
    exitWithError,
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
import Stats.Benchmark (calculateStats, compareBayesian)

-- | Run A/B benchmark comparing candidate against primary target.
runMultiple :: BaselineMode -> TestConfig -> IO RunResult
runMultiple baselineMode cfg = do
    (csvFile, timestamp) <- initOutputFiles

    let epsCandidate = buildEndpoints cfg True
        epsPrimary = buildEndpoints cfg False
        setts = settings cfg

    case (epsCandidate, epsPrimary) of
        ([], _) -> exitWithError $ NoEndpointsError "candidate"
        (_, []) -> exitWithError $ NoEndpointsError "primary"
        _ -> do
            eventChan <- newTChanIO

            let candidateTotal = iterations setts * length epsCandidate
                primaryTotal = iterations setts * length epsPrimary
                numEndpoints = length epsCandidate + length epsPrimary
                targetUrl = candidate (targets cfg) <> " vs " <> primary (targets cfg)
                tuiState = initialState targetUrl candidateTotal numEndpoints

            ctx <- initContext setts csvFile timestamp (Just eventChan) (candidate (targets cfg))

            benchmarkWork <- async $
                (`onException` emitEvent (Just eventChan) BenchmarkFinished) $ do
                    startNs <- getNowNs

                    emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> candidate (git cfg) <> "...")
                    setupOrFail setts (candidate $ git cfg) (candidate $ targets cfg)
                    emitEvent (Just eventChan) (StatusMessage $ "Running " <> candidate (targets cfg) <> " (" <> candidate (git cfg) <> ")")
                    (resultsCandidate, validCandidate) <- benchmarkEndpoints ctx "candidate" epsCandidate

                    emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> primary (git cfg) <> "...")
                    setupOrFail setts (primary $ git cfg) (primary $ targets cfg)
                    emitEvent (Just eventChan) (PhaseStarted primaryTotal)
                    emitEvent (Just eventChan) (StatusMessage $ "Running " <> primary (targets cfg) <> " (" <> primary (git cfg) <> ")")
                    (resultsPrimary, validPrimary) <- benchmarkEndpoints ctx "primary" epsPrimary

                    endNs <- getNowNs

                    emitEvent (Just eventChan) BenchmarkFinished
                    return (resultsCandidate, validCandidate, resultsPrimary, validPrimary, startNs, endNs)

            finalState <- runTUI eventChan tuiState

            if not (finalState ^. tsFinished)
                then do
                    cancel benchmarkWork
                    exitWithError $ EnvironmentSetupError "Benchmark cancelled by user"
                else do
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

    let eps = buildEndpoints cfg True
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
            emitEvent (Just eventChan) (StatusMessage $ "Setting up " <> candidate (git cfg) <> "...")
            setupOrFail setts (candidate $ git cfg) (candidate $ targets cfg)
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
            runTraceAnalysis (rcLogger ctx) (rcManager ctx) setts timestamp startNs endNs

            handleBaseline (rcLogger ctx) baselineMode (T.pack timestamp) stats
