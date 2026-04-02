-- | Benchmark comparison orchestration.
module Runner.Benchmark (runBenchmark, allPairComparisons, withTUI) where

import Benchmark.Config.CLI (BaselineMode (..))
import Benchmark.Config.Loader (buildEndpoints)
import Benchmark.Execution.Environment (runLifecycleSetup, runLifecycleTeardown)
import Benchmark.Report.Baseline (handleBaseline)
import Benchmark.Report.Output (initOutputFiles)
import Benchmark.Reporter (Reporter (..), ReportingContext (..), combineReporters, plotReporter)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), TUIState, initialState, tsError, tsFinished)
import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkConfig (..)
  , BenchmarkStats
  , ChartsSettings
  , LifecycleHooks (..)
  , NamedTarget (..)
  , PerfTestError (..)
  , RegressionResult (..)
  , RunResult (..)
  , Settings (..)
  , ValidationSummary
  , exitWithError
  )
import Benchmark.Types.Error (formatError)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (TBQueue, newTBQueueIO)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as V
import Log (Logger, logInfo, logWarning)
import Runner.Context (RunContext (..), emitEvent, initContext)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (calculateStats, compareBayesian, earthMoversDistance, extractDurations)
import System.Clock (Clock (Realtime), getTime, toNanoSecs)
import System.IO (hIsTerminalDevice, stdin)
import Tracing.Types qualified as TT

-- | Internal config bundle for a single benchmark run (avoids passing 6 positional params).
data BenchmarkRunConfig = BenchmarkRunConfig
  { brcReporter :: Reporter
  , brcBaseline :: BaselineMode
  , brcCharts :: Maybe ChartsSettings
  , brcConfig :: BenchmarkConfig
  , brcCsvFile :: FilePath
  , brcTimestamp :: String
  }

data BenchmarkResult = BenchmarkResult
  { nrName :: Text
  , nrStats :: BenchmarkStats
  , nrDurations :: V.Vector Double
  , nrValidations :: [ValidationSummary]
  , nrStartNs :: TT.Nanoseconds
  , nrEndNs :: TT.Nanoseconds
  }

{-| Run a benchmark action under the TUI, handling async errors and cancellation.
Emits 'BenchmarkFinished' or 'BenchmarkFailed' into the event channel, then
waits for the TUI to exit before returning the action's result.
-}
withTUI :: TBQueue BenchmarkEvent -> TUIState -> IO a -> IO a
withTUI eventChan tuiState work = do
  benchmarkWork <- async $ do
    result <- try work
    case result of
      Right val -> emitEvent (Just eventChan) BenchmarkFinished >> return val
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
    (True, Nothing) -> wait benchmarkWork

-- | Run benchmarks against named targets and compare all pairs.
runBenchmark :: Reporter -> BaselineMode -> Maybe ChartsSettings -> BenchmarkConfig -> IO RunResult
runBenchmark reporter baselineMode mCharts cfg = do
  (csvFile, timestamp) <- initOutputFiles
  let rc =
        BenchmarkRunConfig
          { brcReporter = reporter
          , brcBaseline = baselineMode
          , brcCharts = mCharts
          , brcConfig = cfg
          , brcCsvFile = csvFile
          , brcTimestamp = timestamp
          }
  isTerm <- hIsTerminalDevice stdin
  if isTerm
    then runBenchmarkWithTUI rc
    else runBenchmarkNoTUI rc

-- | Build a 'ReportingContext' from a reporter, baseline mode, and run context.
mkReportingContext :: Reporter -> BaselineMode -> RunContext -> ReportingContext
mkReportingContext reporter baselineMode ctx =
  ReportingContext
    { rctxReporter = reporter
    , rctxBaselineMode = baselineMode
    , rctxLogger = rcLogger ctx
    }

-- | Generate all N*(N-1)/2 pairwise Bayesian comparisons.
allPairComparisons :: [(Text, BenchmarkStats)] -> [(Text, Text, BayesianComparison)]
allPairComparisons [] = []
allPairComparisons ((nameA, statsA) : rest) =
  [ (nameA, nameB, compareBayesian statsA statsB)
  | (nameB, statsB) <- rest
  ]
    ++ allPairComparisons rest

-- | Run benchmarks with TUI display.
runBenchmarkWithTUI :: BenchmarkRunConfig -> IO RunResult
runBenchmarkWithTUI BenchmarkRunConfig {..} = do
  let setts = benchSettings brcConfig
      eps = buildEndpoints "" (NE.fromList (benchPayloads brcConfig))
      numEndpoints = length eps
      perTargetRequests = iterations setts * numEndpoints
  eventChan <- newTBQueueIO 10_000
  let tuiState = initialState "Starting..." perTargetRequests numEndpoints
  ctx <- initContext setts brcCsvFile brcTimestamp (Just eventChan)
  let fullReporter = addPlotReporter (rcLogger ctx) brcCharts brcCsvFile brcReporter
      rctx = mkReportingContext fullReporter brcBaseline ctx
  results <- withTUI eventChan tuiState (runAllTargets ctx brcConfig (Just eventChan))
  postAnalysis rctx ctx brcTimestamp results

-- | Run benchmarks without TUI (headless/CI mode).
runBenchmarkNoTUI :: BenchmarkRunConfig -> IO RunResult
runBenchmarkNoTUI BenchmarkRunConfig {..} = do
  let setts = benchSettings brcConfig
  ctx <- initContext setts brcCsvFile brcTimestamp Nothing
  let fullReporter = addPlotReporter (rcLogger ctx) brcCharts brcCsvFile brcReporter
      rctx = mkReportingContext fullReporter brcBaseline ctx
  results <- runAllTargets ctx brcConfig Nothing
  postAnalysis rctx ctx brcTimestamp results

-- | Optionally combine a plot reporter with the base reporter.
addPlotReporter :: Logger -> Maybe ChartsSettings -> FilePath -> Reporter -> Reporter
addPlotReporter _ Nothing _ r = r
addPlotReporter logger (Just cs) csvFile r = combineReporters [r, plotReporter logger cs csvFile]

-- | Execute benchmarks for all targets sequentially.
runAllTargets :: RunContext -> BenchmarkConfig -> Maybe (TBQueue BenchmarkEvent) -> IO [BenchmarkResult]
runAllTargets ctx cfg eventChan = do
  let setts = benchSettings cfg
  forM (zip [1 ..] (benchTargets cfg)) $ \(idx, t) -> do
    let targetEps = buildEndpoints (targetUrl t) (NE.fromList (benchPayloads cfg))
        totalReqs = iterations setts * length targetEps

    emitEvent eventChan (TargetStarted (targetName t) idx totalReqs)

    when (targetNeedsSetup t) $
      emitEvent eventChan (StatusMessage $ "Setting up " <> targetName t <> "...")
    runLifecycleSetup (rcManager ctx) t >>= either throwIO return

    emitEvent eventChan (StatusMessage $ "Benchmarking " <> targetName t)

    startNs <- fromIntegral . toNanoSecs <$> getTime Realtime
    let ctxWithTarget = ctx {rcTargetName = targetName t}
    (timings, validSummaries) <- benchmarkEndpoints ctxWithTarget (targetName t) targetEps
    endNs <- fromIntegral . toNanoSecs <$> getTime Realtime

    do
      teardownResult <- runLifecycleTeardown t
      case teardownResult of
        Right () -> return ()
        Left err ->
          logWarning (rcLogger ctx) $
            "Teardown failed for " <> targetName t <> ": " <> formatError err

    pure
      BenchmarkResult
        { nrName = targetName t
        , nrStats = calculateStats timings
        , nrDurations = extractDurations timings
        , nrValidations = validSummaries
        , nrStartNs = startNs
        , nrEndNs = endNs
        }

-- | True when a target has a branch or a setup hook that requires pre-benchmark work.
targetNeedsSetup :: NamedTarget -> Bool
targetNeedsSetup t =
  maybe False (not . T.null) (targetBranch t)
    || maybe False (isJust . hookSetup) (targetLifecycle t)

-- | Post-benchmark analysis: reporting, tracing, baselines.
postAnalysis :: ReportingContext -> RunContext -> String -> [BenchmarkResult] -> IO RunResult
postAnalysis rctx ctx timestamp results = do
  let namedStats = Map.fromList [(nrName r, nrStats r) | r <- results]
      durMap = Map.fromList [(nrName r, nrDurations r) | r <- results]
      pairs0 = allPairComparisons [(nrName r, nrStats r) | r <- results]
      pairs = map (attachEMD durMap) pairs0
      validAll = concatMap nrValidations results

  reportBenchmark (rctxReporter rctx) namedStats pairs validAll

  forM_ results $ \r -> do
    logInfo (rctxLogger rctx) $ "\n#----- Traces: " <> nrName r <> " -----#"
    runTraceAnalysis ctx timestamp (nrStartNs r) (nrEndNs r)

  handleBenchmarkBaseline rctx (T.pack timestamp) namedStats

-- | Attach Earth Mover's Distance to a pairwise comparison using stored duration vectors.
attachEMD ::
  Map Text (V.Vector Double) -> (Text, Text, BayesianComparison) -> (Text, Text, BayesianComparison)
attachEMD durMap (a, b, cmp) =
  case (Map.lookup a durMap, Map.lookup b durMap) of
    (Just dA, Just dB) -> (a, b, cmp {emd = Just (earthMoversDistance dA dB)})
    _ -> (a, b, cmp)

-- | Run baseline operations for each target, aggregate results.
handleBenchmarkBaseline :: ReportingContext -> Text -> Map Text BenchmarkStats -> IO RunResult
handleBenchmarkBaseline ReportingContext {rctxBaselineMode = NoBaseline} _ _ = return RunSuccess
handleBenchmarkBaseline rctx timestamp namedStats = do
  results <- forM (Map.toList namedStats) $ \(name, stats) -> do
    let qualifiedMode = qualifyBaselineMode name (rctxBaselineMode rctx)
        qualifiedRctx = rctx {rctxBaselineMode = qualifiedMode}
    handleBaseline qualifiedRctx timestamp stats
  if any isRegression results
    then return $ RunRegression $ aggregateRegressions [r | RunRegression r <- results]
    else return RunSuccess

qualifyBaselineMode :: Text -> BaselineMode -> BaselineMode
qualifyBaselineMode target (SaveBaseline name) = SaveBaseline (name <> "--" <> target)
qualifyBaselineMode target (CompareBaseline name) = CompareBaseline (name <> "--" <> target)
qualifyBaselineMode target (SaveAndCompare s c) = SaveAndCompare (s <> "--" <> target) (c <> "--" <> target)
qualifyBaselineMode _ NoBaseline = NoBaseline

isRegression :: RunResult -> Bool
isRegression (RunRegression _) = True
isRegression _ = False

aggregateRegressions :: [RegressionResult] -> RegressionResult
aggregateRegressions rs =
  RegressionResult
    { regressionBaseline = T.intercalate ", " (map regressionBaseline rs)
    , regressionMetrics = concatMap regressionMetrics rs
    , regressionPassed = all regressionPassed rs
    }
