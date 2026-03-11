module Benchmark.Types.Stats
  ( BenchmarkStats (..)
  , BayesianComparison (..)
  , PercentileComparison (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Descriptive statistics from a benchmark run.
data BenchmarkStats = BenchmarkStats
  { totalRequests :: Int
  , countSuccess :: Int
  , countFailure :: Int
  , meanMs :: Double
  , stdDevMs :: Double
  , minMs :: Double
  , maxMs :: Double
  , p50Ms :: Double
  , p95Ms :: Double
  , p99Ms :: Double
  , esMs :: Double
  -- ^ Expected Shortfall: mean latency of the worst 1% of requests (E[X | X > p99])
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comparison of a percentile between primary and candidate.
data PercentileComparison = PercentileComparison
  { pctDifference :: Double
  , pctCredibleLower :: Double
  , pctCredibleUpper :: Double
  , probPctRegression :: Double
  -- ^ Probability candidate percentile is worse (higher latency)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Bayesian A/B comparison results.
data BayesianComparison = BayesianComparison
  { probBFasterThanA :: Double
  -- ^ Probability candidate is faster than primary (mean-level, based on σ/√n)
  , probSingleRequestFaster :: Double
  -- ^ Probability a single request to B is faster than a single request to A (based on σ)
  , meanDifference :: Double
  -- ^ Primary mean - Candidate mean (positive = candidate faster)
  , credibleIntervalLower :: Double
  , credibleIntervalUpper :: Double
  , effectSize :: Double
  -- ^ Cohen's d
  , relativeEffect :: Double
  -- ^ Percentage improvement (positive = candidate faster)
  , p95Comparison :: PercentileComparison
  , p99Comparison :: PercentileComparison
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
