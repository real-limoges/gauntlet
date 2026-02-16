-- |
-- Module      : Benchmark.Config
-- Description : Configuration loading and validation
-- Stability   : experimental
--
-- Loads JSON configuration and builds endpoint definitions for benchmarking.
module Benchmark.Config
  ( loadConfig,
    buildEndpoints,
    validateConfig,
    toEndpoint,
  )
where

import Benchmark.Types
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)

loadConfig :: FilePath -> IO (Either String TestConfig)
loadConfig path = do
  content <- LBS.readFile path
  return $ eitherDecode content

-- | Build endpoint list from config, selecting primary or candidate target.
-- When useCandidate is True, uses candidate target; otherwise uses primary.
buildEndpoints :: TestConfig -> Bool -> [Endpoint]
buildEndpoints config useCandidate =
  let baseUrl =
        if useCandidate
          then candidate (targets config)
          else primary (targets config)
   in map (toEndpoint baseUrl) (payloads config)

toEndpoint :: Text -> PayloadSpec -> Endpoint
toEndpoint baseUrl spec =
  let customHeaders = maybe [] Map.toList (specHeaders spec)
      -- Only add default Content-Type if not already specified
      defaultHeaders =
        if any (\(k, _) -> k == "Content-Type") customHeaders
          then []
          else [("Content-Type", "application/json")]
   in Endpoint
        { method = specMethod spec,
          url = baseUrl <> specPath spec,
          body = specBody spec,
          headers = defaultHeaders ++ customHeaders
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
