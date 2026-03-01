{-|
Module      : Runner.Nway
Description : N-way benchmark orchestration
Stability   : experimental

Runs benchmarks against N>=2 named targets and produces all-pairs
Bayesian comparisons. Integrates TUI, tracing, baseline, and CSV output.
-}
module Runner.Nway (runNway, allPairComparisons) where

import Benchmark.CLI (BaselineMode (..))
import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initNwayOutputFiles, writeMarkdownReport)
import Benchmark.Report (printNwayReport, printValidationSummary)
import Benchmark.Report.Markdown (markdownNwayReport, markdownValidationReport)
import Benchmark.TUI (runTUI)
import Benchmark.TUI.State (BenchmarkEvent (..), initialState, tsFinished)
import Benchmark.Types
  ( BayesianComparison
  , BenchmarkStats
  , NamedTarget (..)
  , NwayConfig (..)
  , OutputFormat (..)
  , PerfTestError (..)
  , RegressionResult (..)
  , RunResult (..)
  , Settings (..)
  , TestingResponse
  , ValidationSummary
  , exitWithError
  )
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (TChan, newTChanIO)
import Control.Exception (onException)
import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, logInfo)
import Network.HTTP.Client (Manager)
import Runner.Baseline (handleBaseline)
import Runner.Context (RunContext (..), emitEvent, getNowNs, initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Runner.Tracing (runTraceAnalysis)
import Stats.Benchmark (addFrequentistTests, calculateStats, compareBayesian)
import System.IO (hIsTerminalDevice, stdin)
import Tracing.Types qualified as TT

data NwayResult = NwayResult
  { nrName :: Text
  , nrStats :: BenchmarkStats
  , nrResponses :: [TestingResponse]
  , nrValidations :: [ValidationSummary]
  , nrStartNs :: TT.Nanoseconds
  , nrEndNs :: TT.Nanoseconds
  }

-- | Run benchmarks against N named targets and compare all pairs.
runNway :: BaselineMode -> OutputFormat -> NwayConfig -> IO RunResult
runNway baselineMode outFmt cfg = do
  (csvFile, timestamp) <- initNwayOutputFiles

  let setts = nwaySettings cfg
      eps = buildEndpoints "" (nwayPayloads cfg)
      numEndpoints = length eps
      perTargetRequests = iterations setts * numEndpoints

  isTerm <- hIsTerminalDevice stdin
  if isTerm
    then runNwayWithTUI baselineMode outFmt cfg csvFile timestamp setts perTargetRequests numEndpoints
    else runNwayNoTUI baselineMode outFmt cfg csvFile timestamp setts

-- | Generate all N*(N-1)/2 pairwise Bayesian comparisons.
allPairComparisons :: [(Text, BenchmarkStats, [TestingResponse])] -> [(Text, Text, BayesianComparison)]
allPairComparisons [] = []
allPairComparisons ((nameA, statsA, timingsA) : rest) =
  [ (nameA, nameB, comparison)
  | (nameB, statsB, timingsB) <- rest
  , let comparison =
          addFrequentistTests
            timingsA
            timingsB
            (compareBayesian statsA statsB)
  ]
    ++ allPairComparisons rest

-- | Run N-way benchmarks with TUI display.
runNwayWithTUI ::
  BaselineMode ->
  OutputFormat ->
  NwayConfig ->
  FilePath ->
  String ->
  Settings ->
  Int ->
  Int ->
  IO RunResult
runNwayWithTUI baselineMode outFmt cfg csvFile timestamp setts perTargetRequests numEndpoints = do
  eventChan <- newTChanIO
  let tuiState = initialState "Starting..." perTargetRequests numEndpoints
  ctx <- initContext setts csvFile timestamp (Just eventChan)

  benchmarkWork <-
    async $
      (`onException` emitEvent (Just eventChan) BenchmarkFinished) $
        runAllTargets ctx cfg (Just eventChan)

  finalState <- runTUI eventChan tuiState

  if not (tsFinished finalState)
    then do
      cancel benchmarkWork
      exitWithError $ EnvironmentSetupError "Benchmark cancelled by user"
    else do
      results <- wait benchmarkWork
      postAnalysis (rcLogger ctx) (rcManager ctx) baselineMode outFmt setts timestamp results

-- | Run N-way benchmarks without TUI (headless/CI mode).
runNwayNoTUI ::
  BaselineMode ->
  OutputFormat ->
  NwayConfig ->
  FilePath ->
  String ->
  Settings ->
  IO RunResult
runNwayNoTUI baselineMode outFmt cfg csvFile timestamp setts = do
  ctx <- initContext setts csvFile timestamp Nothing
  results <- runAllTargets ctx cfg Nothing
  postAnalysis (rcLogger ctx) (rcManager ctx) baselineMode outFmt setts timestamp results

-- | Execute benchmarks for all targets.
runAllTargets :: RunContext -> NwayConfig -> Maybe (TChan BenchmarkEvent) -> IO [NwayResult]
runAllTargets ctx cfg eventChan = do
  let setts = nwaySettings cfg
  results <- forM (zip [1 ..] (nwayTargets cfg)) $ \(idx, t) -> do
    let n = length (nwayTargets cfg)
        targetEps = buildEndpoints (targetUrl t) (nwayPayloads cfg)

    emitEvent eventChan (TargetStarted (targetName t) idx n)

    case targetBranch t of
      Just branch | not (T.null branch) -> do
        emitEvent eventChan (StatusMessage $ "Setting up " <> branch <> "...")
        setupOrFail setts branch (targetUrl t) Nothing
      _ -> return ()

    emitEvent eventChan (StatusMessage $ "Benchmarking " <> targetName t)

    startNs <- getNowNs
    let ctxWithTarget = ctx {rcTargetName = Just (targetName t)}
    (timings, validSummaries) <- benchmarkEndpoints ctxWithTarget (T.unpack (targetName t)) targetEps
    endNs <- getNowNs

    pure NwayResult
      { nrName = targetName t
      , nrStats = calculateStats timings
      , nrResponses = timings
      , nrValidations = validSummaries
      , nrStartNs = startNs
      , nrEndNs = endNs
      }

  emitEvent eventChan BenchmarkFinished
  return results

-- | Post-benchmark analysis: reporting, tracing, baselines.
postAnalysis ::
  Logger ->
  Manager ->
  BaselineMode ->
  OutputFormat ->
  Settings ->
  String ->
  [NwayResult] ->
  IO RunResult
postAnalysis logger mgr baselineMode outFmt setts timestamp results = do
  let namedStats = [(nrName r, nrStats r) | r <- results]
      pairInput = [(nrName r, nrStats r, nrResponses r) | r <- results]
      pairs = allPairComparisons pairInput
      validAll = concatMap nrValidations results

  printNwayReport namedStats pairs
  printValidationSummary validAll
  writeMarkdownReport outFmt $
    markdownNwayReport namedStats pairs
      <> markdownValidationReport validAll

  forM_ results $ \r -> do
    logInfo logger $ "\n#----- Traces: " <> nrName r <> " -----#"
    runTraceAnalysis logger mgr setts timestamp (nrStartNs r) (nrEndNs r)

  handleNwayBaseline logger baselineMode (T.pack timestamp) namedStats

-- | Run baseline operations for each N-way target, aggregate results.
handleNwayBaseline :: Logger -> BaselineMode -> Text -> [(Text, BenchmarkStats)] -> IO RunResult
handleNwayBaseline _logger NoBaseline _timestamp _namedStats = return RunSuccess
handleNwayBaseline logger mode timestamp namedStats = do
  results <- forM namedStats $ \(name, stats) -> do
    let qualifiedMode = qualifyBaselineMode name mode
    handleBaseline logger qualifiedMode timestamp stats
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
