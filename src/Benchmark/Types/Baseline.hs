module Benchmark.Types.Baseline
  ( Baseline (..)
  , RegressionThresholds (..)
  , RegressionResult (..)
  , MetricRegression (..)
  , RunResult (..)
  , defaultThresholds
  )
where

import Benchmark.Types.Error (PerfTestError)
import Benchmark.Types.Stats (BenchmarkStats)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Saved baseline for regression comparison.
data Baseline = Baseline
  { baselineName :: Text
  , baselineTimestamp :: Text
  , baselineStats :: BenchmarkStats
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Thresholds for regression detection (as fractions, e.g., 0.10 = 10%).
data RegressionThresholds = RegressionThresholds
  { thresholdMean :: Double
  , thresholdP50 :: Double
  , thresholdP95 :: Double
  , thresholdP99 :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default thresholds: 10% for mean/p50/p95, 15% for p99.
defaultThresholds :: RegressionThresholds
defaultThresholds =
  RegressionThresholds
    { thresholdMean = 0.10
    , thresholdP50 = 0.10
    , thresholdP95 = 0.10
    , thresholdP99 = 0.15
    }

-- | A single metric's regression status.
data MetricRegression = MetricRegression
  { metricName :: Text
  , metricBaseline :: Double
  , metricCurrent :: Double
  , metricChange :: Double
  -- ^ Relative change (positive = regression, negative = improvement)
  , metricThreshold :: Double
  , metricRegressed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Result of comparing current run against baseline.
data RegressionResult = RegressionResult
  { regressionBaseline :: Text
  , regressionMetrics :: [MetricRegression]
  , regressionPassed :: Bool
  -- ^ True if no metric exceeded its threshold
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Exit status from a benchmark run.
data RunResult
  = RunSuccess
  | RunRegression RegressionResult
  | RunError PerfTestError
  deriving stock (Show, Eq)
