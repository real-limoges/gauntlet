{-|
Module      : Runner.Baseline
Description : Baseline save/compare operations and regression reporting
-}
module Runner.Baseline (handleBaseline) where

import Benchmark.Baseline (compareToBaseline, loadBaseline, saveBaseline)
import Benchmark.CI (CIMode (..), detectCIMode, formatForCI, writeArtifactReport, writeGitHubStepSummary)
import Benchmark.CLI (BaselineMode (..))
import Benchmark.Output (resultsDir)
import Benchmark.Types
  ( Baseline (..)
  , BenchmarkStats
  , MetricRegression (..)
  , RegressionResult (..)
  , RunResult (..)
  , defaultThresholds
  )
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, logInfo)
import Text.Printf (printf)

-- | Handle baseline save/compare operations per 'BaselineMode'.
handleBaseline :: Logger -> BaselineMode -> Text -> BenchmarkStats -> IO RunResult
handleBaseline logger mode timestamp stats = case mode of
  NoBaseline -> return RunSuccess
  SaveBaseline name -> do
    result <- saveBaseline name timestamp stats
    case result of
      Left err -> do
        logInfo logger $ "Error: " <> T.pack err
        return RunSuccess
      Right path -> do
        logInfo logger $ "Baseline saved: " <> T.pack path
        return RunSuccess
  CompareBaseline name -> do
    result <- loadBaseline name
    case result of
      Left err -> do
        logInfo logger $ "Error: " <> T.pack err
        return RunSuccess
      Right baseline -> doBaselineComparison timestamp stats baseline
  SaveAndCompare saveName compareName -> do
    saveResult <- saveBaseline saveName timestamp stats
    case saveResult of
      Left err -> logInfo logger $ "Warning: Failed to save baseline: " <> T.pack err
      Right path -> logInfo logger $ "Baseline saved: " <> T.pack path
    loadResult <- loadBaseline compareName
    case loadResult of
      Left err -> do
        logInfo logger $ "Error: " <> T.pack err
        return RunSuccess
      Right baseline -> doBaselineComparison timestamp stats baseline

-- | Run baseline comparison and return the 'RunResult'.
doBaselineComparison :: Text -> BenchmarkStats -> Baseline -> IO RunResult
doBaselineComparison timestamp stats baseline = do
  let regression = compareToBaseline defaultThresholds baseline stats
  printRegressionResult regression
  emitCIOutput regression (T.unpack timestamp)
  if regressionPassed regression
    then return RunSuccess
    else return $ RunRegression regression

-- | Emit CI-specific output based on the detected CI environment.
emitCIOutput :: RegressionResult -> String -> IO ()
emitCIOutput regression timestamp = do
  ciMode <- detectCIMode
  case ciMode of
    None -> return ()
    GitLab -> do
      formatForCI GitLab regression
      let reportFile = resultsDir ++ "/benchmark-report-" ++ timestamp ++ ".md"
      writeArtifactReport reportFile regression
    GitHub -> do
      formatForCI GitHub regression
      writeGitHubStepSummary regression
      let reportFile = resultsDir ++ "/benchmark-report-" ++ timestamp ++ ".md"
      writeArtifactReport reportFile regression

-- | Print a formatted regression comparison table.
printRegressionResult :: RegressionResult -> IO ()
printRegressionResult regression = do
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
