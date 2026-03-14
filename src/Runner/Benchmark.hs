-- | Benchmark comparison orchestration.
module Runner.Benchmark (runBenchmark, allPairComparisons) where

import Benchmark.Config.CLI (BaselineMode (..))
import Benchmark.Config.Loader (buildEndpoints)
import Benchmark.Report.Baseline (handleBaseline)
import Benchmark.Report.Output (initOutputFiles)
import Benchmark.Reporter (Reporter (..), combineReporters)
import Benchmark.Reporter.Plot (plotReporter)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState, tsError, tsFinished)
import Benchmark.Types
  ( BayesianComparison
  , BenchmarkConfig (..)
  , BenchmarkStats
  , ChartsSettings
  , NamedTarget (..)
  , PerfTestError (..)
  , RegressionResult (..)
  , RunResult (..)
  , Settings (..)
  , ValidationSummary
  , exitWithError
  )
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (TChan, newTChanIO)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO, try)
import Control.Monad (forM, forM_, void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, logInfo)
import Network.HTTP.Client (Manager)
import Runner.Context (RunContext (..), emitEvent, getNowNs, initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (calculateStats, compareBayesian)
import System.IO (hIsTerminalDevice, stdin)
import Tracing.Types qualified as TT

data BenchmarkResult = BenchmarkResult
  { nrName :: Text
  , nrStats :: BenchmarkStats
  , nrValidations :: [ValidationSummary]
  , nrStartNs :: TT.Nanoseconds
  , nrEndNs :: TT.Nanoseconds
  }

-- | Run benchmarks against named targets and compare all pairs.
runBenchmark :: Reporter -> BaselineMode -> Maybe ChartsSettings -> BenchmarkConfig -> IO RunResult
runBenchmark reporter baselineMode mCharts cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let fullReporter = case mCharts of
        Nothing -> reporter
        Just cs -> combineReporters [reporter, plotReporter cs csvFile]
      setts = benchSettings cfg
      eps = buildEndpoints "" (benchPayloads cfg)
      numEndpoints = length eps
      perTargetRequests = iterations setts * numEndpoints

  isTerm <- hIsTerminalDevice stdin
  if isTerm
    then runBenchmarkWithTUI fullReporter baselineMode cfg csvFile timestamp setts perTargetRequests numEndpoints
    else runBenchmarkNoTUI fullReporter baselineMode cfg csvFile timestamp setts

-- | Generate all N*(N-1)/2 pairwise Bayesian comparisons.
allPairComparisons :: [(Text, BenchmarkStats)] -> [(Text, Text, BayesianComparison)]
allPairComparisons [] = []
allPairComparisons ((nameA, statsA) : rest) =
  [ (nameA, nameB, compareBayesian statsA statsB)
  | (nameB, statsB) <- rest
  ]
    ++ allPairComparisons rest

-- | Run benchmarks with TUI display.
runBenchmarkWithTUI ::
  Reporter ->
  BaselineMode ->
  BenchmarkConfig ->
  FilePath ->
  String ->
  Settings ->
  Int ->
  Int ->
  IO RunResult
runBenchmarkWithTUI reporter baselineMode cfg csvFile timestamp setts perTargetRequests numEndpoints = do
  eventChan <- newTChanIO
  let tuiState = initialState "Starting..." perTargetRequests numEndpoints
  ctx <- initContext setts csvFile timestamp (Just eventChan)

  benchmarkWork <- async $ do
    result <- (try (runAllTargets ctx cfg (Just eventChan)) :: IO (Either SomeException [BenchmarkResult]))
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
      results <- wait benchmarkWork
      postAnalysis reporter (rcLogger ctx) (rcManager ctx) baselineMode setts timestamp results

-- | Run benchmarks without TUI (headless/CI mode).
runBenchmarkNoTUI ::
  Reporter ->
  BaselineMode ->
  BenchmarkConfig ->
  FilePath ->
  String ->
  Settings ->
  IO RunResult
runBenchmarkNoTUI reporter baselineMode cfg csvFile timestamp setts = do
  ctx <- initContext setts csvFile timestamp Nothing
  results <- runAllTargets ctx cfg Nothing
  postAnalysis reporter (rcLogger ctx) (rcManager ctx) baselineMode setts timestamp results

-- | Execute benchmarks for all targets.
runAllTargets :: RunContext -> BenchmarkConfig -> Maybe (TChan BenchmarkEvent) -> IO [BenchmarkResult]
runAllTargets ctx cfg eventChan = do
  let setts = benchSettings cfg
  forM (zip [1 ..] (benchTargets cfg)) $ \(idx, t) -> do
    let targetEps = buildEndpoints (targetUrl t) (benchPayloads cfg)
        totalReqs = iterations setts * length targetEps

    emitEvent eventChan (TargetStarted (targetName t) idx totalReqs)

    case targetBranch t of
      Just branch | not (T.null branch) -> do
        emitEvent eventChan (StatusMessage $ "Setting up " <> branch <> "...")
        setupOrFail (rcManager ctx) setts branch (targetUrl t) (Just ["--profile", "testing", "up", "-d", "--build"])
      _ -> return ()

    emitEvent eventChan (StatusMessage $ "Benchmarking " <> targetName t)

    startNs <- getNowNs
    let ctxWithTarget = ctx {rcTargetName = targetName t}
    (timings, validSummaries) <- benchmarkEndpoints ctxWithTarget (targetName t) targetEps
    endNs <- getNowNs

    pure
      BenchmarkResult
        { nrName = targetName t
        , nrStats = calculateStats timings
        , nrValidations = validSummaries
        , nrStartNs = startNs
        , nrEndNs = endNs
        }

-- | Post-benchmark analysis: reporting, tracing, baselines.
postAnalysis ::
  Reporter ->
  Logger ->
  Manager ->
  BaselineMode ->
  Settings ->
  String ->
  [BenchmarkResult] ->
  IO RunResult
postAnalysis reporter logger mgr baselineMode setts timestamp results = do
  let namedStats = Map.fromList [(nrName r, nrStats r) | r <- results]
      pairs = allPairComparisons [(nrName r, nrStats r) | r <- results]
      validAll = concatMap nrValidations results

  reportBenchmark reporter namedStats pairs validAll

  forM_ results $ \r -> do
    logInfo logger $ "\n#----- Traces: " <> nrName r <> " -----#"
    runTraceAnalysis logger mgr setts timestamp (nrStartNs r) (nrEndNs r)

  handleBenchmarkBaseline reporter logger baselineMode (T.pack timestamp) namedStats

-- | Run baseline operations for each target, aggregate results.
handleBenchmarkBaseline ::
  Reporter -> Logger -> BaselineMode -> Text -> Map Text BenchmarkStats -> IO RunResult
handleBenchmarkBaseline _reporter _logger NoBaseline _timestamp _namedStats = return RunSuccess
handleBenchmarkBaseline reporter logger mode timestamp namedStats = do
  results <- forM (Map.toList namedStats) $ \(name, stats) -> do
    let qualifiedMode = qualifyBaselineMode name mode
    handleBaseline reporter logger qualifiedMode timestamp stats
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
