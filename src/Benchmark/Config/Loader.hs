-- | Configuration loading and validation from JSON/YAML files.
module Benchmark.Config.Loader
  ( loadBenchmarkConfig
  , buildEndpoints
  , validateBenchmarkConfig
  )
where

import Benchmark.Config.Env (interpolateEnv, loadEnvVars)
import Benchmark.Types
import Benchmark.Types.Config (HealthCheckConfig (..), HookCommand (..), LifecycleHooks (..), NamedTarget (..))
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO

-- | Load a benchmark config from a JSON\/YAML file.
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

-- | Validate a 'BenchmarkConfig' (at least one target, non-empty payloads, positive iterations).
validateBenchmarkConfig :: BenchmarkConfig -> Either PerfTestError BenchmarkConfig
validateBenchmarkConfig cfg = do
  when (null (benchPayloads cfg)) $
    Left $
      ConfigValidationError "No payloads defined in config"
  when (iterations (benchSettings cfg) <= 0) $
    Left $
      ConfigValidationError "iterations must be greater than 0"
  validateCommon (benchPayloads cfg) (benchSettings cfg)
  when (null (benchTargets cfg)) $
    Left $
      ConfigValidationError "Must have at least 1 target"
  mapM_ validateTargetLifecycle (benchTargets cfg)
  Right cfg

-- | Shared validation logic for settings and payloads.
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
validateLoadMode (Just (LoadPoissonRpm rpm))
  | rpm <= 0 = Left $ ConfigValidationError "loadMode poisson: target mean must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just LoadUnthrottled) = Right ()
validateLoadMode (Just (LoadConstantRpm rpm))
  | rpm <= 0 = Left $ ConfigValidationError "loadMode constantRpm: targetRpm must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just (LoadRampUp (RampUpConfig {..})))
  | rampStartRpm <= 0 = Left $ ConfigValidationError "loadMode rampUp: startRpm must be greater than 0"
  | rampEndRpm <= 0 = Left $ ConfigValidationError "loadMode rampUp: endRpm must be greater than 0"
  | rampDurationSecs <= 0 = Left $ ConfigValidationError "loadMode rampUp: durationSecs must be greater than 0"
  | otherwise = Right ()
validateLoadMode (Just (LoadStepLoad [])) =
  Left $ ConfigValidationError "loadMode stepLoad: steps must not be empty"
validateLoadMode (Just (LoadStepLoad steps)) = mapM_ validateStep steps
  where
    validateStep s
      | loadStepRpm s <= 0 =
          Left $ ConfigValidationError "loadMode stepLoad: each step rpm must be greater than 0"
      | loadStepDurationSecs s <= 0 =
          Left $ ConfigValidationError "loadMode stepLoad: each step durationSecs must be greater than 0"
      | otherwise = Right ()

-- | Validate lifecycle hooks for a named target.
validateTargetLifecycle :: NamedTarget -> Either PerfTestError ()
validateTargetLifecycle t = case targetLifecycle t of
  Nothing -> Right ()
  Just hooks -> do
    mapM_ (validateHookCommand "setup") (hookSetup hooks)
    mapM_ (validateHookCommand "teardown") (hookTeardown hooks)
    mapM_ validateHealthCheck (hookHealthCheck hooks)

validateHookCommand :: Text -> HookCommand -> Either PerfTestError ()
validateHookCommand label HookCommand{..} = do
  when (T.null (T.strip hookCmd)) $
    Left $ ConfigValidationError $ label <> " hook: cmd must not be empty"
  case hookTimeoutSecs of
    Just t | t <= 0 -> Left $ ConfigValidationError $ label <> " hook: timeoutSecs must be greater than 0"
    _ -> Right ()

validateHealthCheck :: HealthCheckConfig -> Either PerfTestError ()
validateHealthCheck HealthCheckConfig{..} = do
  when (T.null (T.strip hcUrl)) $
    Left $ ConfigValidationError "lifecycle healthCheck: url must not be empty"
  case hcTimeoutSecs of
    Just t | t <= 0 -> Left $ ConfigValidationError "lifecycle healthCheck: timeoutSecs must be greater than 0"
    _ -> Right ()
  case hcIntervalMs of
    Just i | i <= 0 -> Left $ ConfigValidationError "lifecycle healthCheck: intervalMs must be greater than 0"
    _ -> Right ()
