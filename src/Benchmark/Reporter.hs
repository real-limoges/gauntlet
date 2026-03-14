-- | Reporter abstraction for composable benchmark output backends.
module Benchmark.Reporter
  ( Reporter (..)
  , noOpReporter
  , combineReporters
  )
where

import Benchmark.Types
import Data.Map.Strict (Map)
import Data.Text (Text)

-- | Record of callbacks for reporting benchmark results.
data Reporter = Reporter
  { reportSingle :: Text -> BenchmarkStats -> [ValidationSummary] -> IO ()
  , reportBenchmark ::
      Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> [ValidationSummary] -> IO ()
  , reportRegression :: RegressionResult -> IO ()
  }

-- | A reporter that discards all output.
noOpReporter :: Reporter
noOpReporter =
  Reporter
    { reportSingle = \_ _ _ -> pure ()
    , reportBenchmark = \_ _ _ -> pure ()
    , reportRegression = \_ -> pure ()
    }

-- | Sequence two reporters so both receive every event.
combineReporters :: [Reporter] -> Reporter
combineReporters rs =
  Reporter
    { reportSingle = \n s v -> mapM_ (\r -> reportSingle r n s v) rs
    , reportBenchmark = \m ps v -> mapM_ (\r -> reportBenchmark r m ps v) rs
    , reportRegression = \rr -> mapM_ (`reportRegression` rr) rs
    }
