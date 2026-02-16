{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Runner
-- Description : Benchmark orchestration
-- Stability   : experimental
--
-- Orchestrates A/B benchmark runs (runMultiple) and single-target runs (runSingle).
-- Handles environment setup, warmup, concurrent execution, and optional trace analysis.
module Runner (runMultiple, runSingle) where

import Benchmark.Baseline (compareToBaseline, loadBaseline, saveBaseline)
import Benchmark.CI (CIMode (..), detectCIMode, formatForCI, writeArtifactReport)
import Benchmark.CLI (BaselineMode (..))
import Benchmark.Config (buildEndpoints)
import Benchmark.Environment (setupEnvironment)
import Benchmark.Network (addAuth, initNetwork, readToken, runBenchmark, runBenchmarkWithEvents)
import Benchmark.Output (initOutputFiles, resultsDir, writeLatencies)
import Benchmark.Plotting (plotDistributions)
import Benchmark.Report (printMultipleBenchmarkReport, printSingleBenchmarkReport)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState)
import Benchmark.Types (BenchmarkOutput (..), BenchmarkStats (..), Endpoint (..), MetricRegression (..), Nanoseconds (..), OutputConfig (..), OutputFormat (..), PayloadResult (..), PerfTestError (..), RegressionResult (..), RunResult (..), Settings (..), Targets (..), TempoSettings, TestConfig (..), TestingResponse (..), TraceOutput (..), defaultThresholds, defaultWarmupSettings, exitWithError)
import Benchmark.Types qualified as PT
import Control.Concurrent (forkIO, newQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (ToJSON, encode, toJSON)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Fixed qualified as TT
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, logDebug, logError, logInfo, logWarning, makeLogger)
import Network.HTTP.Client (Manager)
import Stats.Benchmark (calculateStats, compareBayesian)
import System.Clock (Clock (Realtime), getTime, toNanoSecs)
import Text.Printf (printf)
import Tracing.Analysis (aggregateBySpanName)
import Tracing.Client (fetchTracesForTimeRange)
import Tracing.Report (printTraceAnalysis, writeRawTraces)
import Tracing.Types (Span (..), TempoConfig, Trace (..), TraceQuery (..))
import Tracing.Types qualified as TT

data RunContext = RunContext
  { rcSettings :: Settings,
    rcManager :: Manager,
    rcToken :: Text,
    rcCsvFile :: FilePath,
    rcTimestamp :: String,
    rcEventChan :: Maybe (TChan BenchmarkEvent),
    rcLogger :: Logger
  }

buildTraceQuery :: TempoSettings -> TT.Nanoseconds -> TT.Nanoseconds -> TraceQuery
buildTraceQuery tempoSetts startNs endNs =
  TraceQuery
    { queryService = PT.tempoServiceName tempoSetts,
      querySpanName = Nothing,
      queryStartNs = startNs,
      queryEndNs = endNs,
      queryLimit = 100,
      queryMinDuration = Nothing
    }

setupOrFail :: Text -> Text -> IO ()
setupOrFail branch target =
  setupEnvironment branch target >>= either exitWithError return

benchmarkEndpoints :: OutputFormat -> RunContext -> String -> [Endpoint] -> IO [TestingResponse]
benchmarkEndpoints outFmt ctx label eps = case eps of
  [] -> exitWithError $ NoEndpointsError label
  (firstEp : _) -> do
    runWarmup outFmt ctx firstEp
    runEndpointLoop outFmt ctx eps

-- | Log message only in terminal mode.
logMsg :: Logger -> OutputFormat -> Text -> IO ()
logMsg logger OutputTerminal msg = logInfo logger msg
logMsg _ OutputJSON _ = return ()

initContext :: Settings -> FilePath -> String -> Maybe (TChan BenchmarkEvent) -> IO RunContext
initContext setts csvFile timestamp eventChan = do
  token <- readToken (T.unpack $ secrets setts) >>= either exitWithError return
  mgr <- initNetwork setts
  let logger = makeLogger (fromMaybe PT.defaultLogLevel (PT.logLevel setts))
  return RunContext {rcSettings = setts, rcManager = mgr, rcToken = token, rcCsvFile = csvFile, rcTimestamp = timestamp, rcEventChan = eventChan, rcLogger = logger}

-- | Run A/B benchmark comparing candidate against primary target.
runMultiple :: OutputFormat -> BaselineMode -> TestConfig -> IO RunResult
runMultiple outFmt baselineMode cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let epsCandidate = buildEndpoints cfg False
      epsPrimary = buildEndpoints cfg True
      setts = settings cfg

  case (epsCandidate, epsPrimary) of
    ([], _) -> exitWithError $ NoEndpointsError "candidate"
    (_, []) -> exitWithError $ NoEndpointsError "primary"
    _ -> case outFmt of
      OutputTerminal -> runMultipleWithTUI baselineMode cfg csvFile timestamp epsCandidate epsPrimary setts
      OutputJSON -> runMultipleJSON baselineMode cfg csvFile timestamp epsCandidate epsPrimary setts

-- | Run A/B benchmark with TUI (terminal mode)
runMultipleWithTUI :: BaselineMode -> TestConfig -> FilePath -> String -> [Endpoint] -> [Endpoint] -> Settings -> IO RunResult
runMultipleWithTUI baselineMode cfg csvFile timestamp epsCandidate epsPrimary setts = do
  eventChan <- newTChanIO

  -- Total = candidate endpoints + primary endpoints
  let totalRequests = iterations setts * (length epsCandidate + length epsPrimary)
      numEndpoints = length epsCandidate + length epsPrimary
      targetUrl = candidate (targets cfg) <> " vs " <> primary (targets cfg)
      tuiState = initialState targetUrl totalRequests numEndpoints

  ctx <- initContext setts csvFile timestamp (Just eventChan)

  resultVar <- newEmptyMVar

  _ <- forkIO $ do
    startNs <- getNowNs

    setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
    resultsCandidate <- benchmarkEndpoints OutputTerminal ctx "candidate" epsCandidate

    setupOrFail (primary $ git cfg) (primary $ targets cfg)
    resultsPrimary <- benchmarkEndpoints OutputTerminal ctx "primary" epsPrimary

    endNs <- getNowNs

    emitEvent (Just eventChan) BenchmarkFinished
    putMVar resultVar (resultsCandidate, resultsPrimary, startNs, endNs)

  _ <- runTUI eventChan tuiState

  (resultsCandidate, resultsPrimary, startNs, endNs) <- takeMVar resultVar

  let statsCandidate = calculateStats resultsCandidate
      statsPrimary = calculateStats resultsPrimary
      bayes = compareBayesian statsPrimary statsCandidate

  printMultipleBenchmarkReport "primary" "candidate" statsPrimary statsCandidate bayes
  let timesPrimary = map (fromIntegral . unNanoseconds . durationNs) resultsPrimary
      timesCandidate = map (fromIntegral . unNanoseconds . durationNs) resultsCandidate
      plotFile = resultsDir ++ "/kde_plot-" ++ timestamp ++ ".png"
  plotDistributions timesPrimary timesCandidate plotFile
  runTraceAnalysis (rcLogger ctx) OutputTerminal (rcManager ctx) setts timestamp startNs endNs

  handleBaseline (rcLogger ctx) OutputTerminal baselineMode (T.pack timestamp) statsCandidate

-- | Run A/B benchmark with JSON output (CI mode)
runMultipleJSON :: BaselineMode -> TestConfig -> FilePath -> String -> [Endpoint] -> [Endpoint] -> Settings -> IO RunResult
runMultipleJSON baselineMode cfg csvFile timestamp epsCandidate epsPrimary setts = do
  ctx <- initContext setts csvFile timestamp Nothing

  startNs <- getNowNs

  logMsg (rcLogger ctx) OutputJSON "Starting with Candidate ..."
  setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
  resultsCandidate <- benchmarkEndpoints OutputJSON ctx "candidate" epsCandidate

  logMsg (rcLogger ctx) OutputJSON "Starting with Primary"
  setupOrFail (primary $ git cfg) (primary $ targets cfg)
  resultsPrimary <- benchmarkEndpoints OutputJSON ctx "primary" epsPrimary

  endNs <- getNowNs

  let statsCandidate = calculateStats resultsCandidate
      statsPrimary = calculateStats resultsPrimary
      bayes = compareBayesian statsPrimary statsCandidate

  traceOut <- getTraceOutput (rcManager ctx) setts startNs endNs
  let result =
        PayloadResult
          { prPayload = "benchmark",
            prPrimary = Just statsPrimary,
            prCandidate = Just statsCandidate,
            prComparison = Just bayes
          }
      output =
        BenchmarkOutput
          { outputTimestamp = T.pack timestamp,
            outputConfig = OutputConfig (iterations setts) (concurrency setts),
            outputResults = [result],
            outputTracing = traceOut
          }
  LBS8.putStrLn (encode output)

  handleBaseline (rcLogger ctx) OutputJSON baselineMode (T.pack timestamp) statsCandidate

-- | Run single-target benchmark without comparison.
runSingle :: OutputFormat -> BaselineMode -> TestConfig -> IO RunResult
runSingle outFmt baselineMode cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let eps = buildEndpoints cfg False
      setts = settings cfg
      targetUrl = candidate $ targets cfg

  case outFmt of
    OutputTerminal -> runSingleWithTUI baselineMode cfg csvFile timestamp eps setts targetUrl
    OutputJSON -> runSingleJSON baselineMode cfg csvFile timestamp eps setts targetUrl

-- | Run single benchmark with TUI (terminal mode)
runSingleWithTUI :: BaselineMode -> TestConfig -> FilePath -> String -> [Endpoint] -> Settings -> Text -> IO RunResult
runSingleWithTUI baselineMode cfg csvFile timestamp eps setts targetUrl = do
  -- Create event channel for TUI communication
  eventChan <- newTChanIO

  -- Calculate total requests for progress tracking
  let totalRequests = iterations setts * length eps
      numEndpoints = length eps
      tuiState = initialState targetUrl totalRequests numEndpoints

  ctx <- initContext setts csvFile timestamp (Just eventChan)

  -- MVar to collect results from benchmark thread
  resultVar <- newEmptyMVar

  -- Fork benchmark to background thread
  _ <- forkIO $ do
    startNs <- getNowNs
    setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
    results <- benchmarkEndpoints OutputTerminal ctx "endpoints" eps
    endNs <- getNowNs

    -- Signal TUI that benchmark is done
    emitEvent (Just eventChan) BenchmarkFinished

    -- Store results for main thread
    putMVar resultVar (results, startNs, endNs)

  -- Run TUI on main thread (blocks until quit or benchmark finished)
  _ <- runTUI eventChan tuiState

  -- Collect results from benchmark thread
  (results, startNs, endNs) <- takeMVar resultVar

  let stats = calculateStats results

  -- Print final report after TUI exits
  printSingleBenchmarkReport targetUrl stats
  runTraceAnalysis (rcLogger ctx) OutputTerminal (rcManager ctx) setts timestamp startNs endNs

  handleBaseline (rcLogger ctx) OutputTerminal baselineMode (T.pack timestamp) stats

-- | Run single benchmark with JSON output (CI mode)
runSingleJSON :: BaselineMode -> TestConfig -> FilePath -> String -> [Endpoint] -> Settings -> Text -> IO RunResult
runSingleJSON baselineMode cfg csvFile timestamp eps setts targetUrl = do
  ctx <- initContext setts csvFile timestamp Nothing

  startNs <- getNowNs
  setupOrFail (candidate $ git cfg) (candidate $ targets cfg)
  results <- benchmarkEndpoints OutputJSON ctx "endpoints" eps
  endNs <- getNowNs

  let stats = calculateStats results

  traceOut <- getTraceOutput (rcManager ctx) setts startNs endNs
  let result =
        PayloadResult
          { prPayload = targetUrl,
            prPrimary = Nothing,
            prCandidate = Just stats,
            prComparison = Nothing
          }
      output =
        BenchmarkOutput
          { outputTimestamp = T.pack timestamp,
            outputConfig = OutputConfig (iterations setts) (concurrency setts),
            outputResults = [result],
            outputTracing = traceOut
          }
  LBS8.putStrLn (encode output)

  handleBaseline (rcLogger ctx) OutputJSON baselineMode (T.pack timestamp) stats

runEndpointLoop :: OutputFormat -> RunContext -> [Endpoint] -> IO [TestingResponse]
runEndpointLoop outFmt RunContext {..} endpoints = do
  let iters = iterations rcSettings
      conc = concurrency rcSettings
  sem <- newQSem conc

  logMsg rcLogger outFmt $ T.pack $ "Starting " ++ show (length endpoints) ++ " endpoints with " ++ show conc ++ " concurrency in parallel..."

  let indexedEndpoints = zip [1 ..] endpoints
      numEndpoints = length endpoints

  results <-
    mapConcurrently
      ( \(idx, ep) -> do
          -- Emit endpoint started event
          emitEvent rcEventChan (EndpointStarted (url ep) idx numEndpoints)

          let authorizedEp = addAuth rcToken ep
          responses <- case rcEventChan of
            Just chan -> runBenchmarkWithEvents rcSettings sem rcManager iters idx authorizedEp chan
            Nothing -> runBenchmark rcSettings sem rcManager iters idx authorizedEp
          return (idx, ep, responses)
      )
      indexedEndpoints

  writeLatencies rcCsvFile results

  return (concatMap (\(_, _, rs) -> rs) results)

-- | Emit event to TUI channel if present
emitEvent :: Maybe (TChan BenchmarkEvent) -> BenchmarkEvent -> IO ()
emitEvent Nothing _ = return ()
emitEvent (Just chan) event = atomically $ writeTChan chan event

runWarmup :: OutputFormat -> RunContext -> Endpoint -> IO ()
runWarmup outFmt RunContext {..} ep = do
  let warmupSettings = maybe defaultWarmupSettings id (PT.warmup rcSettings)
      warmupIters = PT.warmupIterations warmupSettings
  when (warmupIters > 0) $ do
    logMsg rcLogger outFmt $ T.pack $ "Warming up (" ++ show warmupIters ++ " iteration" ++ (if warmupIters == 1 then "" else "s") ++ ")..."
    sem <- newQSem 1
    let authorizedEp = addAuth rcToken ep
    _ <- runBenchmark rcSettings sem rcManager warmupIters 1 authorizedEp
    return ()

getNowNs :: IO TT.Nanoseconds
getNowNs = fromIntegral . toNanoSecs <$> getTime Realtime

toTempoConfig :: TempoSettings -> TempoConfig
toTempoConfig ts =
  TT.TempoConfig
    { TT.tempoUrl = PT.tempoUrl ts,
      TT.tempoServiceName = PT.tempoServiceName ts,
      TT.tempoEnabled = fromMaybe True (PT.tempoEnabled ts),
      TT.tempoAuthToken = PT.tempoAuthToken ts
    }

runTraceAnalysis :: Logger -> OutputFormat -> Manager -> Settings -> String -> TT.Nanoseconds -> TT.Nanoseconds -> IO ()
runTraceAnalysis logger outFmt mgr setts timestamp startNs endNs = case tempo setts of
  Nothing -> return ()
  Just tempoSetts
    | not (fromMaybe True (PT.tempoEnabled tempoSetts)) -> return ()
    | otherwise -> do
        logMsg logger outFmt "\n#----- Fetching Traces from Tempo -----#"
        let cfg = toTempoConfig tempoSetts
            query = buildTraceQuery tempoSetts startNs endNs
        result <- try (fetchTracesForTimeRange mgr cfg query) :: IO (Either SomeException (Either String [Trace]))
        case result of
          Left err -> logWarning logger $ T.pack $ "Failed to fetch traces: " ++ show err
          Right (Left err) -> logWarning logger $ T.pack $ "Tempo error: " ++ err
          Right (Right []) -> logWarning logger "No traces found for time range"
          Right (Right traces) -> do
            let tracesFile = resultsDir ++ "/traces-" ++ timestamp ++ ".json"
            writeRawTraces tracesFile traces
            logInfo logger $ T.pack $ "Raw traces written to: " ++ tracesFile
            printTraceAnalysis traces

-- | Get trace output for JSON mode.
getTraceOutput :: Manager -> Settings -> TT.Nanoseconds -> TT.Nanoseconds -> IO (Maybe TraceOutput)
getTraceOutput mgr setts startNs endNs = case tempo setts of
  Nothing -> return Nothing
  Just tempoSetts
    | not (fromMaybe True (PT.tempoEnabled tempoSetts)) -> return Nothing
    | otherwise -> do
        let cfg = toTempoConfig tempoSetts
            query = buildTraceQuery tempoSetts startNs endNs
        result <- try (fetchTracesForTimeRange mgr cfg query) :: IO (Either SomeException (Either String [Trace]))
        case result of
          Left _ -> return Nothing
          Right (Left _) -> return Nothing
          Right (Right []) -> return Nothing
          Right (Right traces) -> do
            let allSpans = concatMap traceSpans traces
                aggs = aggregateBySpanName allSpans
            return $
              Just
                TraceOutput
                  { traceCount = length traces,
                    traceSpanAggregations = toJSON aggs
                  }

-- | Handle baseline save/compare operations.
handleBaseline :: Logger -> OutputFormat -> BaselineMode -> Text -> BenchmarkStats -> IO RunResult
handleBaseline logger outFmt mode timestamp stats = case mode of
  NoBaseline -> return RunSuccess
  SaveBaseline name -> do
    result <- saveBaseline name timestamp stats
    case result of
      Left err -> do
        logMsg logger outFmt $ T.pack $ "Error: " ++ err
        return RunSuccess -- Save failure is not a regression
      Right path -> do
        logMsg logger outFmt $ T.pack $ "Baseline saved: " ++ path
        return RunSuccess
  CompareBaseline name -> do
    result <- loadBaseline name
    case result of
      Left err -> do
        logMsg logger outFmt $ T.pack $ "Error: " ++ err
        return RunSuccess -- Missing baseline is not a regression
      Right baseline -> do
        let regression = compareToBaseline defaultThresholds baseline stats
        printRegressionResult outFmt regression
        emitCIOutput regression (T.unpack timestamp)
        if regressionPassed regression
          then return RunSuccess
          else return $ RunRegression regression
  SaveAndCompare saveName compareName -> do
    -- First save
    saveResult <- saveBaseline saveName timestamp stats
    case saveResult of
      Left err -> logMsg logger outFmt $ T.pack $ "Warning: Failed to save baseline: " ++ err
      Right path -> logMsg logger outFmt $ T.pack $ "Baseline saved: " ++ path

    -- Then compare
    loadResult <- loadBaseline compareName
    case loadResult of
      Left err -> do
        logMsg logger outFmt $ T.pack $ "Error: " ++ err
        return RunSuccess
      Right baseline -> do
        let regression = compareToBaseline defaultThresholds baseline stats
        printRegressionResult outFmt regression
        emitCIOutput regression (T.unpack timestamp)
        if regressionPassed regression
          then return RunSuccess
          else return $ RunRegression regression

-- | Emit CI-specific output when running in GitLab.
emitCIOutput :: RegressionResult -> String -> IO ()
emitCIOutput regression timestamp = do
  ciMode <- detectCIMode
  case ciMode of
    None -> return ()
    GitLab -> do
      formatForCI GitLab regression
      let reportFile = resultsDir ++ "/benchmark-report-" ++ timestamp ++ ".md"
      writeArtifactReport reportFile regression

-- | Print regression comparison results.
printRegressionResult :: OutputFormat -> RegressionResult -> IO ()
printRegressionResult OutputJSON regression = LBS8.putStrLn (encode regression)
printRegressionResult OutputTerminal regression = do
  putStrLn ""
  putStrLn $ "#----- Regression Check vs '" ++ T.unpack (regressionBaseline regression) ++ "' -----#"
  putStrLn ""
  printf
    "%-8s %12s %12s %10s %10s %s\n"
    ("Metric" :: String)
    ("Baseline" :: String)
    ("Current" :: String)
    ("Change" :: String)
    ("Threshold" :: String)
    ("Status" :: String)
  putStrLn $ replicate 70 '-'

  mapM_ printMetric (regressionMetrics regression)

  putStrLn ""
  if regressionPassed regression
    then putStrLn "Result: PASSED (no regressions detected)"
    else putStrLn "Result: FAILED (regression detected)"

printMetric :: MetricRegression -> IO ()
printMetric m =
  printf
    "%-8s %10.2f ms %10.2f ms %+9.1f%% %9.0f%% %s\n"
    (T.unpack $ metricName m)
    (metricBaseline m)
    (metricCurrent m)
    (metricChange m * 100)
    (metricThreshold m * 100)
    (if metricRegressed m then "REGRESSED" else "ok" :: String)
