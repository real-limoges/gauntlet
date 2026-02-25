{-|
Module      : Benchmark.Baseline
Description : Baseline storage and regression detection
Stability   : experimental

Save benchmark results as baselines and compare future runs against them
for CI/CD regression detection.
-}
module Benchmark.Baseline
  ( saveBaseline
  , loadBaseline
  , compareToBaseline
  , baselineDir
  , listBaselines
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, (</>))

import Benchmark.Types
  ( Baseline (..)
  , BenchmarkStats (..)
  , MetricRegression (..)
  , RegressionResult (..)
  , RegressionThresholds (..)
  )

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

-- | List all available baseline names.
listBaselines :: IO [Text]
listBaselines = do
  createDirectoryIfMissing True baselineDir
  files <- listDirectory baselineDir
  return $ map (T.pack . takeBaseName) files

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
