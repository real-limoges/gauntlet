module ConfigSpec (configSpec) where

import Benchmark.Config
import Benchmark.Types
import Data.Map.Strict qualified as Map
import TastyCompat (shouldBe, shouldContain, shouldNotContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers

configSpec :: TestTree
configSpec =
  testGroup
    "Benchmark.Config"
    [ testGroup
        "validateConfig"
        [ testCase "accepts valid configuration" $ do
            let cfg = makeValidConfig
            validateConfig cfg `shouldBe` Right cfg
        , testCase "rejects empty payloads" $ do
            let cfg = makeValidConfig {payloads = []}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldBe` "No payloads defined in config"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero iterations" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {iterations = 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldBe` "iterations must be greater than 0"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects negative iterations" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {iterations = -5}}
            case validateConfig cfg of
              Left (ConfigValidationError _) -> pure ()
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero concurrency" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {concurrency = 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldBe` "concurrency must be greater than 0"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects invalid HTTP methods" $ do
            let badPayload = PayloadSpec "test" "INVALID" "/path" Nothing Nothing Nothing
            let cfg = makeValidConfig {payloads = [badPayload]}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "Invalid HTTP method"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts all valid HTTP methods" $ do
            let methods = ["GET", "POST", "PUT", "DELETE", "PATCH"]
            let mkPayload m = PayloadSpec "test" m "/path" Nothing Nothing Nothing
            let cfg = makeValidConfig {payloads = map mkPayload methods}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "rejects negative requestTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {requestTimeout = Just (-1)}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "requestTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero requestTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {requestTimeout = Just 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "requestTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts positive requestTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {requestTimeout = Just 30}}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "rejects zero healthCheckTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {healthCheckTimeout = Just 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "healthCheckTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects retryBackoffMultiplier < 1.0" $ do
            let r = RetrySettings 3 1000 0.5
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "retryBackoffMultiplier"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects negative retryMaxAttempts" $ do
            let r = RetrySettings (-1) 1000 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "retryMaxAttempts"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero retryInitialDelayMs" $ do
            let r = RetrySettings 3 0 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "retryInitialDelayMs"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts valid retry settings" $ do
            let r = RetrySettings 3 1000 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "accepts missing optional settings" $ do
            -- Default makeValidConfig has all optional fields as Nothing
            validateConfig makeValidConfig `shouldBe` Right makeValidConfig
        ]
    , testGroup
        "buildEndpoints"
        [ testCase "builds endpoints for primary target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints (primary (targets cfg)) (payloads cfg)
            case endpoints of
              [ep] -> url ep `shouldBe` "http://primary.test/api/test"
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "builds endpoints for candidate target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints (candidate (targets cfg)) (payloads cfg)
            case endpoints of
              [ep] -> url ep `shouldBe` "http://candidate.test/api/test"
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "preserves method and body from payload spec" $ do
            let cfg = makeValidConfig
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> do
                method ep `shouldBe` "POST"
                body ep `shouldBe` Nothing
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "sets content-type header" $ do
            let cfg = makeValidConfig
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> headers ep `shouldContain` [("Content-Type", "application/json")]
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "includes custom headers from PayloadSpec" $ do
            let customHeaders = Map.fromList [("X-API-Key", "secret"), ("X-Request-ID", "123")]
            let payload = PayloadSpec "test" "GET" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig {payloads = [payload]}
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> do
                headers ep `shouldContain` [("X-API-Key", "secret")]
                headers ep `shouldContain` [("X-Request-ID", "123")]
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "merges custom headers with default Content-Type" $ do
            let customHeaders = Map.fromList [("X-Custom", "value")]
            let payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig {payloads = [payload]}
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> do
                headers ep `shouldContain` [("Content-Type", "application/json")]
                headers ep `shouldContain` [("X-Custom", "value")]
                length (headers ep) `shouldBe` 2
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "overrides Content-Type with custom value" $ do
            let customHeaders = Map.fromList [("Content-Type", "text/xml")]
            let payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig {payloads = [payload]}
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> do
                headers ep `shouldContain` [("Content-Type", "text/xml")]
                headers ep `shouldNotContain` [("Content-Type", "application/json")]
                length (filter (\(k, _) -> k == "Content-Type") (headers ep)) `shouldBe` 1
              _ -> assertFailure "Expected exactly one endpoint"
        , testCase "handles empty custom headers" $ do
            let payload = PayloadSpec "test" "GET" "/api" Nothing (Just Map.empty) Nothing
            let cfg = makeValidConfig {payloads = [payload]}
            case buildEndpoints (primary (targets cfg)) (payloads cfg) of
              [ep] -> headers ep `shouldBe` [("Content-Type", "application/json")]
              _ -> assertFailure "Expected exactly one endpoint"
        ]
    ]
