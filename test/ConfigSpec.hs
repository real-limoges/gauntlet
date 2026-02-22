module ConfigSpec (configSpec) where

import Benchmark.Config
import Benchmark.Types
import Data.Map.Strict qualified as Map
import Test.Hspec
import TestHelpers

configSpec :: Spec
configSpec = describe "Benchmark.Config" $ do
    describe "validateConfig" $ do
        it "accepts valid configuration" $ do
            let cfg = makeValidConfig
            validateConfig cfg `shouldBe` Right cfg

        it "rejects empty payloads" $ do
            let cfg = makeValidConfig{payloads = []}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "No payloads defined in config"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects zero iterations" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){iterations = 0}}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "iterations must be greater than 0"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects negative iterations" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){iterations = -5}}
            case validateConfig cfg of
                Left (ConfigValidationError _) -> pure ()
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects zero concurrency" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){concurrency = 0}}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "concurrency must be greater than 0"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects invalid HTTP methods" $ do
            let badPayload = PayloadSpec "test" "INVALID" "/path" Nothing Nothing Nothing
            let cfg = makeValidConfig{payloads = [badPayload]}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldContain` "Invalid HTTP method"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "accepts all valid HTTP methods" $ do
            let methods = ["GET", "POST", "PUT", "DELETE", "PATCH"]
            let mkPayload m = PayloadSpec "test" m "/path" Nothing Nothing Nothing
            let cfg = makeValidConfig{payloads = map mkPayload methods}
            validateConfig cfg `shouldBe` Right cfg

    describe "buildEndpoints" $ do
        it "builds endpoints for primary target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints cfg False
            case endpoints of
                [ep] -> url ep `shouldBe` "http://primary.test/api/test"
                _ -> expectationFailure "Expected exactly one endpoint"

        it "builds endpoints for candidate target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints cfg True
            case endpoints of
                [ep] -> url ep `shouldBe` "http://candidate.test/api/test"
                _ -> expectationFailure "Expected exactly one endpoint"

        it "preserves method and body from payload spec" $ do
            let cfg = makeValidConfig
            case buildEndpoints cfg False of
                [ep] -> do
                    method ep `shouldBe` "POST"
                    body ep `shouldBe` Nothing
                _ -> expectationFailure "Expected exactly one endpoint"

        it "sets content-type header" $ do
            let cfg = makeValidConfig
            case buildEndpoints cfg False of
                [ep] -> headers ep `shouldContain` [("Content-Type", "application/json")]
                _ -> expectationFailure "Expected exactly one endpoint"

        it "includes custom headers from PayloadSpec" $ do
            let customHeaders = Map.fromList [("X-API-Key", "secret"), ("X-Request-ID", "123")]
            let payload = PayloadSpec "test" "GET" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig{payloads = [payload]}
            case buildEndpoints cfg False of
                [ep] -> do
                    headers ep `shouldContain` [("X-API-Key", "secret")]
                    headers ep `shouldContain` [("X-Request-ID", "123")]
                _ -> expectationFailure "Expected exactly one endpoint"

        it "merges custom headers with default Content-Type" $ do
            let customHeaders = Map.fromList [("X-Custom", "value")]
            let payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig{payloads = [payload]}
            case buildEndpoints cfg False of
                [ep] -> do
                    headers ep `shouldContain` [("Content-Type", "application/json")]
                    headers ep `shouldContain` [("X-Custom", "value")]
                    length (headers ep) `shouldBe` 2
                _ -> expectationFailure "Expected exactly one endpoint"

        it "overrides Content-Type with custom value" $ do
            let customHeaders = Map.fromList [("Content-Type", "text/xml")]
            let payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
            let cfg = makeValidConfig{payloads = [payload]}
            case buildEndpoints cfg False of
                [ep] -> do
                    headers ep `shouldContain` [("Content-Type", "text/xml")]
                    headers ep `shouldNotContain` [("Content-Type", "application/json")]
                    length (filter (\(k, _) -> k == "Content-Type") (headers ep)) `shouldBe` 1
                _ -> expectationFailure "Expected exactly one endpoint"

        it "handles empty custom headers" $ do
            let payload = PayloadSpec "test" "GET" "/api" Nothing (Just Map.empty) Nothing
            let cfg = makeValidConfig{payloads = [payload]}
            case buildEndpoints cfg False of
                [ep] -> headers ep `shouldBe` [("Content-Type", "application/json")]
                _ -> expectationFailure "Expected exactly one endpoint"

    describe "retry settings" $ do
        it "uses default retry settings when not specified" $ do
            let cfg = makeValidConfig
            let setts = settings cfg
            retry setts `shouldBe` Nothing

        it "accepts valid retry configuration" $ do
            let retrySettings = RetrySettings 5 500 2.5
            let setts = (settings makeValidConfig){retry = Just retrySettings}
            let cfg = makeValidConfig{settings = setts}
            case retry (settings cfg) of
                Just rs -> do
                    retryMaxAttempts rs `shouldBe` 5
                    retryInitialDelayMs rs `shouldBe` 500
                    retryBackoffMultiplier rs `shouldBe` 2.5
                Nothing -> expectationFailure "Expected retry settings"

        it "accepts retry with zero attempts (disable retry)" $ do
            let retrySettings = RetrySettings 0 1000 2.0
            let setts = (settings makeValidConfig){retry = Just retrySettings}
            let cfg = makeValidConfig{settings = setts}
            case retry (settings cfg) of
                Just rs -> retryMaxAttempts rs `shouldBe` 0
                Nothing -> expectationFailure "Expected retry settings"

        it "accepts various backoff multipliers" $ do
            let retrySettings = RetrySettings 3 1000 1.5
            let setts = (settings makeValidConfig){retry = Just retrySettings}
            let cfg = makeValidConfig{settings = setts}
            case retry (settings cfg) of
                Just rs -> retryBackoffMultiplier rs `shouldBe` 1.5
                Nothing -> expectationFailure "Expected retry settings"

    describe "warmup settings" $ do
        it "uses default warmup settings when not specified" $ do
            let cfg = makeValidConfig
            warmup (settings cfg) `shouldBe` Nothing

        it "accepts valid warmup configuration" $ do
            let warmupSettings = WarmupSettings 5
            let setts = (settings makeValidConfig){warmup = Just warmupSettings}
            let cfg = makeValidConfig{settings = setts}
            case warmup (settings cfg) of
                Just ws -> warmupIterations ws `shouldBe` 5
                Nothing -> expectationFailure "Expected warmup settings"

        it "accepts warmup with zero iterations (disabled)" $ do
            let warmupSettings = WarmupSettings 0
            let setts = (settings makeValidConfig){warmup = Just warmupSettings}
            let cfg = makeValidConfig{settings = setts}
            case warmup (settings cfg) of
                Just ws -> warmupIterations ws `shouldBe` 0
                Nothing -> expectationFailure "Expected warmup settings"

        it "accepts large warmup iteration counts" $ do
            let warmupSettings = WarmupSettings 1000
            let setts = (settings makeValidConfig){warmup = Just warmupSettings}
            let cfg = makeValidConfig{settings = setts}
            case warmup (settings cfg) of
                Just ws -> warmupIterations ws `shouldBe` 1000
                Nothing -> expectationFailure "Expected warmup settings"
