{-|
Module      : Benchmark.Types
Description : Core data types for benchmarking (re-export facade)
Stability   : experimental

Re-exports all types from submodules. Downstream code can continue to
import 'Benchmark.Types' without changes.
-}
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
  , MWUResult (..)
  , KSResult (..)
  , ADResult (..)

    -- * Verification
  , JsonDiff (..)
  , VerificationResult (..)

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

    -- * Baseline / CI
  , Baseline (..)
  , RegressionThresholds (..)
  , RegressionResult (..)
  , MetricRegression (..)
  , RunResult (..)
  , defaultThresholds

    -- * Output Format
  , OutputFormat (..)

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
import Benchmark.Types.Verify
