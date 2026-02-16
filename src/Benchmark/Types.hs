-- |
-- Module      : Benchmark.Types
-- Description : Core data types for benchmarking
-- Stability   : experimental
--
-- Defines configuration, response, and result types used throughout
-- the benchmark tool.
module Benchmark.Types
  ( -- * Request/Response Types
    Endpoint (..),
    TestingResponse (..),

    -- * Unit Types
    Nanoseconds (..),
    Milliseconds (..),
    nsToMs,

    -- * Statistics Types
    BenchmarkStats (..),
    BayesianComparison (..),
    PercentileComparison (..),

    -- * Verification
    VerificationResult (..),

    -- * Configuration
    TestConfig (..),
    Targets (..),
    Settings (..),
    RetrySettings (..),
    defaultRetrySettings,
    WarmupSettings (..),
    defaultWarmupSettings,
    TempoSettings (..),
    PayloadSpec (..),
    LogLevel (..),
    defaultLogLevel,

    -- * Output Format
    OutputFormat (..),
    BenchmarkOutput (..),
    OutputConfig (..),
    PayloadResult (..),
    TraceOutput (..),

    -- * Baseline / CI
    Baseline (..),
    RegressionThresholds (..),
    RegressionResult (..),
    MetricRegression (..),
    RunResult (..),
    defaultThresholds,

    -- * Error Handling
    PerfTestError (..),
    formatError,
    exitWithError,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, defaultOptions, fieldLabelModifier, genericParseJSON, object, (.=))
import Data.Aeson.Diff (Patch)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (PrintfArg (..))

data Endpoint = Endpoint
  { method :: Text,
    url :: Text,
    body :: Maybe Value,
    headers :: [(Text, Text)]
  }
  deriving stock (Show, Eq)

-- | Duration in nanoseconds with type safety.
newtype Nanoseconds = Nanoseconds {unNanoseconds :: Word64}
  deriving newtype (Show, Read, Eq, Ord, Num, Integral, Real, Enum, FromJSON, ToJSON)

-- | Duration in milliseconds with type safety.
newtype Milliseconds = Milliseconds {unMilliseconds :: Double}
  deriving newtype (Show, Eq, Ord, Num, FromJSON, ToJSON, Real, Fractional, RealFrac, Floating)

instance PrintfArg Milliseconds where
  formatArg (Milliseconds x) = formatArg x

-- | Convert nanoseconds to milliseconds.
nsToMs :: Nanoseconds -> Milliseconds
nsToMs (Nanoseconds ns) = Milliseconds (fromIntegral ns / 1_000_000)

data TestingResponse = TestingResponse
  { durationNs :: Nanoseconds,
    statusCode :: Int,
    respBody :: Maybe LBS.ByteString,
    errorMessage :: Maybe String
  }
  deriving stock (Show, Eq)

-- | Descriptive statistics from a benchmark run.
data BenchmarkStats = BenchmarkStats
  { totalRequests :: Int,
    countSuccess :: Int,
    countFailure :: Int,
    meanMs :: Double,
    stdDevMs :: Double,
    minMs :: Double,
    maxMs :: Double,
    p50Ms :: Double,
    p95Ms :: Double,
    p99Ms :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comparison of a percentile between primary and candidate.
data PercentileComparison = PercentileComparison
  { pctDifference :: Double,
    pctCredibleLower :: Double,
    pctCredibleUpper :: Double,
    -- | Probability candidate percentile is worse (higher latency)
    probPctRegression :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Bayesian A/B comparison results.
data BayesianComparison = BayesianComparison
  { -- | Probability candidate is faster than primary
    probBFasterThanA :: Double,
    -- | Primary mean - Candidate mean (positive = candidate faster)
    meanDifference :: Double,
    credibleIntervalLower :: Double,
    credibleIntervalUpper :: Double,
    -- | Cohen's d
    effectSize :: Double,
    -- | Percentage improvement (positive = candidate faster)
    relativeEffect :: Double,
    p95Comparison :: PercentileComparison,
    p99Comparison :: PercentileComparison
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data VerificationResult
  = Match
  | StatusMismatch Int Int
  | BodyMismatch Patch
  | InvalidJSON String
  deriving stock (Show, Eq, Generic)

data PerfTestError
  = ConfigParseError String
  | ConfigValidationError String
  | TokenReadError FilePath String
  | HealthCheckTimeout Text Int
  | NoEndpointsError String
  | EnvironmentSetupError String
  deriving stock (Show, Eq)

formatError :: PerfTestError -> String
formatError (ConfigParseError msg) = "Failed to parse config: " ++ msg
formatError (ConfigValidationError msg) = "Invalid config: " ++ msg
formatError (TokenReadError path msg) = "Failed to read token from " ++ path ++ ": " ++ msg
formatError (HealthCheckTimeout url retries) = "Service at " ++ T.unpack url ++ " failed to start after " ++ show retries ++ " retries"
formatError (NoEndpointsError which) = "No " ++ which ++ " endpoints defined"
formatError (EnvironmentSetupError msg) = "Environment setup failed: " ++ msg

exitWithError :: PerfTestError -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ formatError err) >> exitFailure

data TestConfig = TestConfig
  { targets :: Targets,
    git :: Targets,
    settings :: Settings,
    payloads :: [PayloadSpec]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Primary and candidate target URLs or git branches.
data Targets = Targets
  { primary :: Text,
    candidate :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Retry configuration for failed requests.
data RetrySettings = RetrySettings
  { -- | Maximum retry attempts (default: 3)
    retryMaxAttempts :: Int,
    -- | Initial delay in milliseconds (default: 1000)
    retryInitialDelayMs :: Int,
    -- | Delay multiplier for exponential backoff (default: 2.0)
    retryBackoffMultiplier :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default retry settings: 3 attempts, 1s initial delay, 2x backoff.
defaultRetrySettings :: RetrySettings
defaultRetrySettings =
  RetrySettings
    { retryMaxAttempts = 3,
      retryInitialDelayMs = 1000,
      retryBackoffMultiplier = 2.0
    }

-- | Warmup configuration before benchmark runs.
data WarmupSettings = WarmupSettings
  { -- | Number of warmup requests per endpoint (default: 1)
    warmupIterations :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default warmup settings: 1 iteration.
defaultWarmupSettings :: WarmupSettings
defaultWarmupSettings = WarmupSettings {warmupIterations = 1}

-- | Log levels for controlling output verbosity.
data LogLevel
  = -- | Most verbose: all messages including detailed debug info
    Debug
  | -- | Normal: informational messages and above
    Info
  | -- | Warnings and errors only
    Warning
  | -- | Errors only
    Error
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON LogLevel where
  parseJSON = \case
    "debug" -> pure Debug
    "info" -> pure Info
    "warning" -> pure Warning
    "error" -> pure Error
    other -> fail $ "Invalid log level: " ++ show other

instance ToJSON LogLevel where
  toJSON Debug = "debug"
  toJSON Info = "info"
  toJSON Warning = "warning"
  toJSON Error = "error"

-- | Default log level: Info
defaultLogLevel :: LogLevel
defaultLogLevel = Info

data Settings = Settings
  { iterations :: Int,
    concurrency :: Int,
    secrets :: Text,
    maxConnections :: Maybe Int,
    connIdleTimeout :: Maybe Int,
    -- | Request timeout in seconds (default: 30)
    requestTimeout :: Maybe Int,
    -- | Retry configuration (default: 3 attempts, 1s delay, 2x backoff)
    retry :: Maybe RetrySettings,
    -- | Warmup configuration (default: 1 iteration)
    warmup :: Maybe WarmupSettings,
    -- | Logging verbosity (default: Info)
    logLevel :: Maybe LogLevel,
    tempo :: Maybe TempoSettings
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Optional Tempo integration settings.
data TempoSettings = TempoSettings
  { -- | Base URL (e.g., "http://tempo:3200")
    tempoUrl :: Text,
    -- | Service name to filter traces
    tempoServiceName :: Text,
    -- | Defaults to True if tempo section present
    tempoEnabled :: Maybe Bool,
    -- | Optional Bearer token
    tempoAuthToken :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data PayloadSpec = PayloadSpec
  { specName :: Text,
    specMethod :: Text,
    specPath :: Text,
    specBody :: Maybe Value,
    specHeaders :: Maybe (Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PayloadSpec where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \x -> case x of
            "specName" -> "name"
            "specMethod" -> "method"
            "specPath" -> "path"
            "specBody" -> "body"
            "specHeaders" -> "headers"
            _ -> x
        }

-- | Output format for benchmark results.
data OutputFormat
  = OutputTerminal
  | OutputJSON
  deriving stock (Show, Eq)

-- | Complete benchmark output for JSON serialization.
data BenchmarkOutput = BenchmarkOutput
  { outputTimestamp :: Text,
    outputConfig :: OutputConfig,
    outputResults :: [PayloadResult],
    outputTracing :: Maybe TraceOutput
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Simplified config info for output.
data OutputConfig = OutputConfig
  { outIterations :: Int,
    outConcurrency :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON OutputConfig where
  toJSON cfg =
    object
      [ "iterations" .= outIterations cfg,
        "concurrency" .= outConcurrency cfg
      ]

-- | Result for a single payload (or A/B comparison).
data PayloadResult = PayloadResult
  { prPayload :: Text,
    prPrimary :: Maybe BenchmarkStats,
    prCandidate :: Maybe BenchmarkStats,
    prComparison :: Maybe BayesianComparison
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PayloadResult where
  toJSON pr =
    object $
      ["payload" .= prPayload pr]
        ++ maybe [] (\s -> ["primary" .= s]) (prPrimary pr)
        ++ maybe [] (\s -> ["candidate" .= s]) (prCandidate pr)
        ++ maybe [] (\c -> ["comparison" .= c]) (prComparison pr)

-- | Trace analysis output.
data TraceOutput = TraceOutput
  { traceCount :: Int,
    traceSpanAggregations :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Saved baseline for regression comparison.
data Baseline = Baseline
  { baselineName :: Text,
    baselineTimestamp :: Text,
    baselineStats :: BenchmarkStats
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Thresholds for regression detection (as fractions, e.g., 0.10 = 10%).
data RegressionThresholds = RegressionThresholds
  { thresholdMean :: Double,
    thresholdP50 :: Double,
    thresholdP95 :: Double,
    thresholdP99 :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default thresholds: 10% for mean/p50/p95, 15% for p99.
defaultThresholds :: RegressionThresholds
defaultThresholds =
  RegressionThresholds
    { thresholdMean = 0.10,
      thresholdP50 = 0.10,
      thresholdP95 = 0.10,
      thresholdP99 = 0.15
    }

-- | A single metric's regression status.
data MetricRegression = MetricRegression
  { metricName :: Text,
    metricBaseline :: Double,
    metricCurrent :: Double,
    -- | Relative change (positive = regression, negative = improvement)
    metricChange :: Double,
    metricThreshold :: Double,
    metricRegressed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Result of comparing current run against baseline.
data RegressionResult = RegressionResult
  { regressionBaseline :: Text,
    regressionMetrics :: [MetricRegression],
    -- | True if no metric exceeded its threshold
    regressionPassed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Exit status from a benchmark run.
data RunResult
  = RunSuccess
  | RunRegression RegressionResult
  | RunError PerfTestError
  deriving stock (Show, Eq)
