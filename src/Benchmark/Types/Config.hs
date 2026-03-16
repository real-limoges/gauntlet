-- | Configuration data types: test settings, targets, payloads, and load modes.
module Benchmark.Types.Config
  ( -- * Configuration
    TestConfig (..)
  , BenchmarkConfig (..)
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
  , RampUpConfig (..)
  , totalRequestsForMode
  , isDurationBased
  , loadModeDurationSecs

    -- * Output Format
  , OutputFormat (..)

    -- * Charts
  , ChartsSettings (..)
  )
where

import Benchmark.Types.Internal (dropFieldPrefix)
import Benchmark.Types.Response (ValidationSpec)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Top-level A\/B benchmark configuration (two targets, git branches, settings, payloads).
data TestConfig = TestConfig
  { targets :: Targets
  , git :: Targets
  , settings :: Settings
  , payloads :: [PayloadSpec]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Benchmark configuration (one or more named targets).
data BenchmarkConfig = BenchmarkConfig
  { benchTargets :: [NamedTarget]
  , benchSettings :: Settings
  , benchPayloads :: [PayloadSpec]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BenchmarkConfig where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropFieldPrefix "bench"
        }

-- | Primary and candidate target URLs or git branches.
data Targets = Targets
  { primary :: Text
  , candidate :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Single named target in a benchmark run
data NamedTarget = NamedTarget
  { targetName :: Text
  , targetUrl :: Text
  , targetBranch :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON NamedTarget where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropFieldPrefix "target"
        }

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
newtype WarmupSettings = WarmupSettings
  { warmupIterations :: Int
  -- ^ Number of warmup requests per endpoint (default: 1)
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

-- | A single step in a step-load profile.
data LoadStep = LoadStep
  { loadStepRps :: Double
  -- ^ Target RPS for this step
  , loadStepDurationSecs :: Double
  -- ^ Duration of this step in seconds
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LoadStep where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropFieldPrefix "loadStep"
        }

instance ToJSON LoadStep where
  toJSON =
    Data.Aeson.genericToJSON
      defaultOptions
        { fieldLabelModifier = dropFieldPrefix "loadStep"
        }

-- | Ramp-up load configuration.
data RampUpConfig = RampUpConfig
  { rampStartRps :: Double
  -- ^ Starting RPS at the beginning of the ramp
  , rampEndRps :: Double
  -- ^ Target RPS at the end of the ramp
  , rampDurationSecs :: Double
  -- ^ Duration of the ramp in seconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Load control mode for pacing request dispatch.
data LoadMode
  = -- | No throttling — fire as fast as concurrency allows (default)
    LoadUnthrottled
  | -- | Constant RPS — pace iterations at a fixed rate
    LoadConstantRps Double
  | -- | Ramp-up — linearly increase RPS from start to end over a duration
    LoadRampUp RampUpConfig
  | -- | Step load — sequential steps, each with its own RPS and duration
    LoadStepLoad [LoadStep]
  | -- | Poisson-distributed load
    LoadPoissonRps Double
  deriving stock (Show, Eq)

instance FromJSON LoadMode where
  parseJSON = withObject "LoadMode" $ \o -> do
    mode <- o .: "mode" :: Parser Text
    case mode of
      "unthrottled" -> pure LoadUnthrottled
      "constantRps" -> LoadConstantRps <$> o .: "targetRps"
      "rampUp" -> LoadRampUp <$> (RampUpConfig <$> o .: "startRps" <*> o .: "endRps" <*> o .: "durationSecs")
      "stepLoad" -> LoadStepLoad <$> o .: "steps"
      "poissonRps" -> LoadPoissonRps <$> o .: "targetRps"
      _ -> fail $ "Unknown load mode: " ++ show mode

instance ToJSON LoadMode where
  toJSON LoadUnthrottled = object ["mode" .= ("unthrottled" :: Text)]
  toJSON (LoadConstantRps rps) = object ["mode" .= ("constantRps" :: Text), "targetRps" .= rps]
  toJSON (LoadRampUp RampUpConfig {..}) =
    object
      [ "mode" .= ("rampUp" :: Text)
      , "startRps" .= rampStartRps
      , "endRps" .= rampEndRps
      , "durationSecs" .= rampDurationSecs
      ]
  toJSON (LoadStepLoad steps) = object ["mode" .= ("stepLoad" :: Text), "steps" .= steps]
  toJSON (LoadPoissonRps rps) = object ["mode" .= ("poissonRps" :: Text), "targetRps" .= rps]

-- | Compute total requests for a load mode given a fallback iteration count.
totalRequestsForMode :: LoadMode -> Int -> Int
totalRequestsForMode LoadUnthrottled fallback = fallback
totalRequestsForMode (LoadConstantRps _) fallback = fallback
totalRequestsForMode (LoadPoissonRps _) fallback = fallback
totalRequestsForMode (LoadRampUp RampUpConfig {..}) _ =
  round ((rampStartRps + rampEndRps) / 2 * rampDurationSecs)
totalRequestsForMode (LoadStepLoad steps) _ =
  sum [round (loadStepRps s * loadStepDurationSecs s) | s <- steps]

-- | Whether the load mode is duration-based (ignores iterations).
isDurationBased :: LoadMode -> Bool
isDurationBased (LoadRampUp {}) = True
isDurationBased (LoadStepLoad _) = True
isDurationBased _ = False

-- | Total duration in seconds for duration-based modes.
loadModeDurationSecs :: LoadMode -> Double
loadModeDurationSecs (LoadRampUp RampUpConfig {..}) = rampDurationSecs
loadModeDurationSecs (LoadStepLoad steps) = sum (map loadStepDurationSecs steps)
loadModeDurationSecs _ = 0

-- | Runtime settings controlling iterations, concurrency, timeouts, and optional features.
data Settings = Settings
  { iterations :: Int
  , concurrency :: Int
  , secrets :: Maybe Text
  , maxConnections :: Maybe Int
  , requestTimeout :: Maybe Int
  -- ^ Request timeout in seconds (default: 30)
  , retry :: Maybe RetrySettings
  -- ^ Retry configuration (default: 3 attempts, 1s delay, 2x backoff)
  , warmup :: Maybe WarmupSettings
  -- ^ Warmup configuration (default: 1 iteration)
  , logLevel :: Maybe LogLevel
  -- ^ Logging verbosity (default: Info)
  , tempo :: Maybe TempoSettings
  , healthCheckPath :: Maybe Text
  -- ^ Health check path appended to service URL (default: "/health")
  , healthCheckTimeout :: Maybe Int
  -- ^ Health check poll timeout in seconds (default: 60)
  , loadMode :: Maybe LoadMode
  -- ^ Load control mode (default: unthrottled)
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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Specification for a single HTTP payload within a benchmark.
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
        { fieldLabelModifier = dropFieldPrefix "spec"
        }

-- | Output format for benchmark reports.
data OutputFormat
  = -- | Terminal-only output (default)
    OutputTerminal
  | -- | Also write a markdown report to the given file path
    OutputMarkdown FilePath
  deriving stock (Show, Eq)

-- | Settings for chart generation via the plot reporter.
data ChartsSettings = ChartsSettings
  { chartsTypes :: [Text]
  -- ^ Chart types to generate (e.g. ["kde", "cdf"]) or ["all"]
  , chartsDir :: Maybe FilePath
  -- ^ Output directory; Nothing = same directory as CSV
  }
  deriving stock (Show, Eq)
