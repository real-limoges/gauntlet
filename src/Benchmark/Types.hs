-- | Re-exports all benchmark type definitions.
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
  , ComparisonReport (..)

    -- * Validation
  , FieldAssertion (..)
  , ValidationSpec (..)
  , ValidationError (..)
  , ValidationSummary (..)

    -- * Configuration
  , BenchmarkConfig (..)
  , NamedTarget (..)
  , LifecycleHooks (..)
  , HookCommand (..)
  , HealthCheckConfig (..)
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
  , RampUpConfig (..)
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
