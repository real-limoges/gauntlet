{- |
Module      : Benchmark.Types
Description : Core data types for benchmarking
Stability   : experimental

Defines configuration, response, and result types used throughout
the benchmark tool.
-}
module Benchmark.Types (
    -- * Request/Response Types
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

    -- * Validation
    FieldAssertion (..),
    ValidationSpec (..),
    ValidationError (..),
    ValidationSummary (..),

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

    -- * Baseline / CI
    Baseline (..),
    RegressionThresholds (..),
    RegressionResult (..),
    MetricRegression (..),
    RunResult (..),
    defaultThresholds,

    -- * Output Format
    OutputFormat (..),

    -- * Error Handling
    PerfTestError (..),
    formatError,
    exitWithError,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, defaultOptions, fieldLabelModifier, genericParseJSON, object, withObject, (.:?), (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (PrintfArg (..))

-- | Per-field assertion in a validation spec.
data FieldAssertion
    = -- | Assert the key exists (any value)
      FieldPresent
    | -- | Assert exact value match
      FieldEq Value
    deriving stock (Show, Eq, Generic)

instance FromJSON FieldAssertion where
    parseJSON = withObject "FieldAssertion" $ \o -> do
        mpresent <- o .:? "present"
        meq <- o .:? "eq"
        case (mpresent :: Maybe Bool, meq :: Maybe Value) of
            (Just True, _) -> pure FieldPresent
            (_, Just v) -> pure (FieldEq v)
            _ -> fail "FieldAssertion must have 'present: true' or 'eq: <value>'"

instance ToJSON FieldAssertion where
    toJSON FieldPresent = object ["present" .= True]
    toJSON (FieldEq v) = object ["eq" .= v]

-- | Declarative validation rules applied to every response for an endpoint.
data ValidationSpec = ValidationSpec
    { validateStatus :: Maybe Int
    -- ^ Expected HTTP status code
    , validateFields :: Maybe (Map Text FieldAssertion)
    -- ^ Map from dot-path (e.g. "$.user.id") to assertion
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON ValidationSpec where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \x -> case x of
                    "validateStatus" -> "status"
                    "validateFields" -> "fields"
                    _ -> x
                }

instance ToJSON ValidationSpec where
    toJSON spec =
        object $
            maybe [] (\s -> ["status" .= s]) (validateStatus spec)
                ++ maybe [] (\f -> ["fields" .= f]) (validateFields spec)

-- | A single validation failure on a response.
data ValidationError
    = -- | Status code did not match: expected, actual
      StatusCodeMismatch Int Int
    | -- | Field path was not found in the response body
      FieldNotFound Text
    | -- | Field path found but value differed: path, expected, actual
      FieldValueMismatch Text Value Value
    | -- | Response body was absent or not valid JSON
      BodyNotJSON
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

-- | Aggregate validation results for all responses from one endpoint.
data ValidationSummary = ValidationSummary
    { totalValidated :: Int
    , totalFailed :: Int
    , validationErrors :: [ValidationError]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data Endpoint = Endpoint
    { method :: Text
    , url :: Text
    , body :: Maybe Value
    , headers :: [(Text, Text)]
    , validate :: Maybe ValidationSpec
    -- ^ Optional per-response validation rules
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
    { durationNs :: Nanoseconds
    , statusCode :: Int
    , respBody :: Maybe LBS.ByteString
    , errorMessage :: Maybe String
    }
    deriving stock (Show, Eq)

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
    -- ^ Probability candidate is faster than primary
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

data VerificationResult
    = Match
    | StatusMismatch Int Int
    | BodyMismatch Text
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
    { targets :: Targets
    , git :: Targets
    , settings :: Settings
    , payloads :: [PayloadSpec]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

-- | Primary and candidate target URLs or git branches.
data Targets = Targets
    { primary :: Text
    , candidate :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

-- | Retry configuration for failed requests.
data RetrySettings = RetrySettings
    { retryMaxAttempts :: Int
    -- ^ Maximum retry attempts (default: 3)
    , retryInitialDelayMs :: Int
    -- ^ Initial delay in milliseconds (default: 1000)
    , retryBackoffMultiplier :: Double
    -- ^ Delay multiplier for exponential backoff (default: 2.0)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Default retry settings: 3 attempts, 1s initial delay, 2x backoff.
defaultRetrySettings :: RetrySettings
defaultRetrySettings =
    RetrySettings
        { retryMaxAttempts = 3
        , retryInitialDelayMs = 1000
        , retryBackoffMultiplier = 2.0
        }

-- | Warmup configuration before benchmark runs.
data WarmupSettings = WarmupSettings
    { warmupIterations :: Int
    -- ^ Number of warmup requests per endpoint (default: 1)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Default warmup settings: 1 iteration.
defaultWarmupSettings :: WarmupSettings
defaultWarmupSettings = WarmupSettings{warmupIterations = 1}

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
    { iterations :: Int
    , concurrency :: Int
    , secrets :: Text
    , maxConnections :: Maybe Int
    , connIdleTimeout :: Maybe Int
    , requestTimeout :: Maybe Int
    -- ^ Request timeout in seconds (default: 30)
    , retry :: Maybe RetrySettings
    -- ^ Retry configuration (default: 3 attempts, 1s delay, 2x backoff)
    , warmup :: Maybe WarmupSettings
    -- ^ Warmup configuration (default: 1 iteration)
    , logLevel :: Maybe LogLevel
    -- ^ Logging verbosity (default: Info)
    , tempo :: Maybe TempoSettings
    , httpVersion :: Maybe Text
    -- ^ HTTP version: "1.1" (default) or "2" (HTTP/2 over TLS)
    , healthCheckPath :: Maybe Text
    -- ^ Health check path appended to service URL (default: "/health")
    , healthCheckTimeout :: Maybe Int
    -- ^ Health check poll timeout in seconds (default: 60)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

-- | Optional Tempo integration settings.
data TempoSettings = TempoSettings
    { tempoUrl :: Text
    -- ^ Base URL (e.g., "http://tempo:3200")
    , tempoServiceName :: Text
    -- ^ Service name to filter traces
    , tempoEnabled :: Maybe Bool
    -- ^ Defaults to True if tempo section present
    , tempoAuthToken :: Maybe Text
    -- ^ Optional Bearer token
    , tempoQueryLimit :: Maybe Int
    -- ^ Maximum number of traces to fetch (default: 100)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data PayloadSpec = PayloadSpec
    { specName :: Text
    , specMethod :: Text
    , specPath :: Text
    , specBody :: Maybe Value
    , specHeaders :: Maybe (Map Text Text)
    , specValidate :: Maybe ValidationSpec
    -- ^ Optional validation rules for responses from this endpoint
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
                    "specValidate" -> "validate"
                    _ -> x
                }

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

-- | Output format for benchmark reports.
data OutputFormat
    = -- | Terminal-only output (default)
      OutputTerminal
    | -- | Also write a markdown report to the given file path
      OutputMarkdown FilePath
    deriving stock (Show, Eq)

-- | Exit status from a benchmark run.
data RunResult
    = RunSuccess
    | RunRegression RegressionResult
    | RunError PerfTestError
    deriving stock (Show, Eq)
