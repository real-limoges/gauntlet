module Benchmark.Types
  ( -- * Request/Response Types
    Endpoint (..)
  , TestingResponse (..)

    -- * Unit Types
  , Nanoseconds (..)
  , Milliseconds (..)
  , nsToMs

    -- * Statistics Types
  , BenchmarkStats (..)
  , BayesianComparison (..)
  , PercentileComparison (..)

    -- * Validation
  , FieldAssertion (..)
  , ValidationSpec (..)
  , ValidationError (..)
  , ValidationSummary (..)

    -- * Configuration
  , TestConfig (..)
  , NwayConfig (..)
  , Targets (..)
  , NamedTarget (..)
  , Settings (..)
  , RetrySettings (..)
  , defaultRetrySettings
  , WarmupSettings (..)
  , defaultWarmupSettings
  , TempoSettings (..)
  , PayloadSpec (..)
  , LogLevel (..)
  , defaultLogLevel

    -- * Load Control
  , LoadMode (..)
  , LoadStep (..)
  , totalRequestsForMode
  , isDurationBased
  , loadModeDurationSecs

    -- * Baseline / CI
  , Baseline (..)
  , RegressionThresholds (..)
  , RegressionResult (..)
  , MetricRegression (..)
  , RunResult (..)
  , defaultThresholds

    -- * Output Format
  , OutputFormat (..)
  , ChartsSettings (..)

    -- * Error Handling
  , PerfTestError (..)
  , formatError
  , exitWithError
  )
where

import Benchmark.Types.Baseline
import Benchmark.Types.Config
import Benchmark.Types.Error
import Benchmark.Types.Response
import Benchmark.Types.Stats
import Benchmark.Types.Units
