-- | Configuration loading and validation from JSON/YAML files.
module Benchmark.Config.Loader
  ( loadConfig
  , loadBenchmarkConfig
  , buildEndpoints
  , validateConfig
  , validateBenchmarkConfig
  )
where

import Benchmark.Config.Env (interpolateEnv, loadEnvVars)
import Benchmark.Types
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO

-- | Load and validate an A\/B benchmark config from a JSON\/YAML file.
loadConfig :: FilePath -> IO (Either String TestConfig)
loadConfig = loadConfigAs

-- | Load and validate a benchmark config from a JSON\/YAML file.
loadBenchmarkConfig :: FilePath -> IO (Either String BenchmarkConfig)
loadBenchmarkConfig = loadConfigAs

loadConfigAs :: FromJSON a => FilePath -> IO (Either String a)
loadConfigAs path = do
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  case result of
    Left err -> return $ Left (show err)
    Right content -> do
      env <- loadEnvVars
      case interpolateEnv env content of
        Left err -> return $ Left err
        Right interpolated -> return $ eitherDecode (LBS.fromStrict (encodeUtf8 interpolated))

{-| Build endpoint list from config, selecting primary or candidate target.
When useCandidate is True, uses candidate target; otherwise uses primary.
-}
buildEndpoints :: Text -> [PayloadSpec] -> [Endpoint]
buildEndpoints baseUrl = map (toEndpoint baseUrl)

toEndpoint :: Text -> PayloadSpec -> Endpoint
toEndpoint baseUrl spec =
  let customHeaders = maybe [] Map.toList (specHeaders spec)
      -- Only add default Content-Type if not already specified
      defaultHeaders =
        [("Content-Type", "application/json") | not (any (\(k, _) -> k == "Content-Type") customHeaders)]
   in Endpoint
        { method = specMethod spec
        , url = baseUrl <> specPath spec
        , body = specBody spec
        , headers = defaultHeaders ++ customHeaders
        , validate = specValidate spec
        }

-- | Validate an A\/B 'TestConfig' (positive iterations, non-empty payloads, etc.).
validateConfig :: TestConfig -> Either PerfTestError TestConfig
validateConfig cfg = do
  when (null (payloads cfg)) $
    Left $
      ConfigValidationError "No payloads defined in config"
  when (iterations (settings cfg) <= 0) $
    Left $
      ConfigValidationError "iterations must be greater than 0"
  validateCommon (payloads cfg) (settings cfg)
  Right cfg

-- | Validate a 'BenchmarkConfig' (at least one target, non-empty payloads).
validateBenchmarkConfig :: BenchmarkConfig -> Either PerfTestError BenchmarkConfig
validateBenchmarkConfig cfg = do
  when (null (benchPayloads cfg)) $
    Left $
      ConfigValidationError "No payloads defined in config"
  validateCommon (benchPayloads cfg) (benchSettings cfg)
  when (null (benchTargets cfg)) $
    Left $
      ConfigValidationError "Must have at least 1 target"
  Right cfg

-- | Shared validation logic for both single and multi-target configs.
validateCommon :: [PayloadSpec] -> Settings -> Either PerfTestError ()
validateCommon ps setts = do
  when (concurrency setts <= 0) $
    Left $
      ConfigValidationError "concurrency must be greater than 0"
  unless (all isValidMethod ps) $
    Left $
      ConfigValidationError "Invalid HTTP method in payloads (must be GET, POST, PUT, DELETE, or PATCH)"
  validateSettings setts

isValidMethod :: PayloadSpec -> Bool
isValidMethod p = specMethod p `elem` ["GET", "POST", "PUT", "DELETE", "PATCH"]

-- | Validate optional settings fields when present.
validateSettings :: Settings -> Either PerfTestError ()
validateSettings setts = do
  check (requestTimeout setts) (<= 0) "requestTimeout must be greater than 0"
  check (healthCheckTimeout setts) (<= 0) "healthCheckTimeout must be greater than 0"
  case retry setts of
    Nothing -> Right ()
    Just r -> do
      when (retryMaxAttempts r < 0) $
        Left $
          ConfigValidationError "retryMaxAttempts must not be negative"
      when (retryInitialDelayMs r <= 0) $
        Left $
          ConfigValidationError "retryInitialDelayMs must be greater than 0"
      when (retryBackoffMultiplier r < 1.0) $
        Left $
          ConfigValidationError "retryBackoffMultiplier must be at least 1.0"
  validateLoadMode (loadMode setts)
  where
    check :: Maybe Int -> (Int -> Bool) -> Text -> Either PerfTestError ()
    check Nothing _ _ = Right ()
    check (Just v) predicate msg
      | predicate v = Left $ ConfigValidationError msg
      | otherwise = Right ()

-- | Validate load mode settings when present.
validateLoadMode :: Maybe LoadMode -> Either PerfTestError ()
validateLoadMode Nothing = Right ()
validateLoadMode (Just (LoadPoissonRps rps))
  | rps <= 0 = Left $ ConfigValidationError "loadMode poisson: target mean must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just LoadUnthrottled) = Right ()
validateLoadMode (Just (LoadConstantRps rps))
  | rps <= 0 = Left $ ConfigValidationError "loadMode constantRps: targetRps must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just (LoadRampUp (RampUpConfig {..})))
  | rampStartRps <= 0 = Left $ ConfigValidationError "loadMode rampUp: startRps must be greater than 0"
  | rampEndRps <= 0 = Left $ ConfigValidationError "loadMode rampUp: endRps must be greater than 0"
  | rampDurationSecs <= 0 = Left $ ConfigValidationError "loadMode rampUp: durationSecs must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just (LoadStepLoad [])) =
  Left $ ConfigValidationError "loadMode stepLoad: steps must not be empty"
validateLoadMode (Just (LoadStepLoad steps)) = mapM_ validateStep steps
  where
    validateStep s
      | loadStepRps s <= 0 =
          Left $ ConfigValidationError "loadMode stepLoad: each step rps must be greater than 0"
      | loadStepDurationSecs s <= 0 =
          Left $ ConfigValidationError "loadMode stepLoad: each step durationSecs must be greater than 0"
      | otherwise = Right ()
