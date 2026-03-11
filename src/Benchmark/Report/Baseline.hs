module Benchmark.Report.Baseline
  ( saveBaseline
  , loadBaseline
  , compareToBaseline
  , baselineDir
  , listBaselines
  , handleBaseline
  , printRegressionResult
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Printf (printf)

import Benchmark.Config.CLI (BaselineMode (..))
import Benchmark.Reporter (Reporter (..))
import Benchmark.Types
  ( Baseline (..)
  , BenchmarkStats (..)
  , MetricRegression (..)
  , RegressionResult (..)
  , RegressionThresholds (..)
  , RunResult (..)
  , defaultThresholds
  )
import Log (Logger, logInfo)

-- | Directory where baselines are stored.
baselineDir :: FilePath
baselineDir = "baselines"

-- | Save current benchmark stats as a named baseline.
saveBaseline :: Text -> Text -> BenchmarkStats -> IO (Either String FilePath)
saveBaseline name timestamp stats = do
  createDirectoryIfMissing True baselineDir
  let baseline =
        Baseline
          { baselineName = name
          , baselineTimestamp = timestamp
          , baselineStats = stats
          }
      path = baselineDir </> T.unpack name ++ ".json"
  result <- try (encodeFile path baseline) :: IO (Either SomeException ())
  case result of
    Left err -> return $ Left $ "Failed to save baseline: " ++ show err
    Right () -> return $ Right path

-- | Load a named baseline from disk.
loadBaseline :: Text -> IO (Either String Baseline)
loadBaseline name = do
  let path = baselineDir </> T.unpack name ++ ".json"
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "Baseline not found: " ++ path
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> return $ Left $ "Failed to parse baseline: " ++ err
        Right baseline -> return $ Right baseline

-- | Compare current stats against a baseline using given thresholds.
compareToBaseline :: RegressionThresholds -> Baseline -> BenchmarkStats -> RegressionResult
compareToBaseline thresholds baseline current =
  let baseStats = baselineStats baseline
      metrics =
        [ checkMetric "mean" (thresholdMean thresholds) (meanMs baseStats) (meanMs current)
        , checkMetric "p50" (thresholdP50 thresholds) (p50Ms baseStats) (p50Ms current)
        , checkMetric "p95" (thresholdP95 thresholds) (p95Ms baseStats) (p95Ms current)
        , checkMetric "p99" (thresholdP99 thresholds) (p99Ms baseStats) (p99Ms current)
        ]
      passed = not (any metricRegressed metrics)
   in RegressionResult
        { regressionBaseline = baselineName baseline
        , regressionMetrics = metrics
        , regressionPassed = passed
        }

-- | List all available baseline names.
listBaselines :: IO [Text]
listBaselines = do
  createDirectoryIfMissing True baselineDir
  files <- listDirectory baselineDir
  return $ map (T.pack . takeBaseName) files

-- | Check if a single metric has regressed beyond threshold.
checkMetric :: Text -> Double -> Double -> Double -> MetricRegression
checkMetric name threshold baselineVal currentVal =
  let change =
        if baselineVal == 0
          then if currentVal == 0 then 0 else 1.0
          else (currentVal - baselineVal) / baselineVal
      regressed = change > threshold
   in MetricRegression
        { metricName = name
        , metricBaseline = baselineVal
        , metricCurrent = currentVal
        , metricChange = change
        , metricThreshold = threshold
        , metricRegressed = regressed
        }

-- ---------------------------------------------------------------------------
-- Baseline handler (orchestration)
-- ---------------------------------------------------------------------------

-- | Handle baseline save/compare operations per 'BaselineMode'.
handleBaseline :: Reporter -> Logger -> BaselineMode -> Text -> BenchmarkStats -> IO RunResult
handleBaseline reporter logger mode timestamp stats = case mode of
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
      Right baseline -> doBaselineComparison reporter stats baseline
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
      Right baseline -> doBaselineComparison reporter stats baseline

doBaselineComparison :: Reporter -> BenchmarkStats -> Baseline -> IO RunResult
doBaselineComparison reporter stats baseline = do
  let regression = compareToBaseline defaultThresholds baseline stats
  reportRegression reporter regression
  if regressionPassed regression
    then return RunSuccess
    else return $ RunRegression regression

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
