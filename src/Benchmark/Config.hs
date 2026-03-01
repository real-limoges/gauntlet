{-|
Module      : Benchmark.Config
Description : Configuration loading and validation
Stability   : experimental

Loads JSON configuration and builds endpoint definitions for benchmarking.
-}
module Benchmark.Config
  ( loadConfig
  , loadNwayConfig
  , buildEndpoints
  , validateConfig
  , validateNwayConfig
  , toEndpoint
  )
where

import Benchmark.Env (interpolateEnv, loadEnvVars)
import Benchmark.Types
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO

loadConfig :: FilePath -> IO (Either String TestConfig)
loadConfig path = do
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  case result of
    Left err -> return $ Left (show err)
    Right content -> do
      env <- loadEnvVars
      case interpolateEnv env content of
        Left err -> return $ Left err
        Right interpolated -> return $ eitherDecode (LBS.fromStrict (encodeUtf8 interpolated))

loadNwayConfig :: FilePath -> IO (Either String NwayConfig)
loadNwayConfig path = do
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

validateConfig :: TestConfig -> Either PerfTestError TestConfig
validateConfig cfg
  | null (payloads cfg) =
      Left $ ConfigValidationError "No payloads defined in config"
  | iterations (settings cfg) <= 0 =
      Left $ ConfigValidationError "iterations must be greater than 0"
  | concurrency (settings cfg) <= 0 =
      Left $ ConfigValidationError "concurrency must be greater than 0"
  | not (all validMethod (payloads cfg)) =
      Left $ ConfigValidationError "Invalid HTTP method in payloads (must be GET, POST, PUT, DELETE, or PATCH)"
  | otherwise = Right cfg
  where
    validMethod p = specMethod p `elem` ["GET", "POST", "PUT", "DELETE", "PATCH"]

validateNwayConfig :: NwayConfig -> Either PerfTestError NwayConfig
validateNwayConfig cfg
  | null (nwayPayloads cfg) =
      Left $ ConfigValidationError "No payloads defined in config"
  | concurrency (nwaySettings cfg) <= 0 =
      Left $ ConfigValidationError "concurrency must be greater than 0"
  | not (all validMethod (nwayPayloads cfg)) =
      Left $ ConfigValidationError "Invalid HTTP method in payloads (must be GET, POST, PUT, DELETE, or PATCH)"
  | length (nwayTargets cfg) < 2 =
      Left $ ConfigValidationError "Must have at least 2 targets"
  | otherwise = Right cfg
  where
    validMethod p = specMethod p `elem` ["GET", "POST", "PUT", "DELETE", "PATCH"]
