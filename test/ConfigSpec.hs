module ConfigSpec (configSpec) where

import Benchmark.Config.Loader
import Benchmark.Types
import Data.Aeson (eitherDecode)
import Data.Map.Strict qualified as Map
import TastyCompat (shouldBe, shouldContain, shouldNotContain, textShouldContain)
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
        , testCase "rejects zero or negative iterations" $ do
            let check n = case validateConfig (makeValidConfig {settings = (settings makeValidConfig) {iterations = n}}) of
                  Left (ConfigValidationError _) -> pure ()
                  _ -> assertFailure $ "Expected ConfigValidationError for iterations=" ++ show n
            check 0
            check (-5)
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
                msg `textShouldContain` "Invalid HTTP method"
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
                msg `textShouldContain` "requestTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero requestTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {requestTimeout = Just 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `textShouldContain` "requestTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts positive requestTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {requestTimeout = Just 30}}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "rejects zero healthCheckTimeout" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {healthCheckTimeout = Just 0}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `textShouldContain` "healthCheckTimeout"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects retryBackoffMultiplier < 1.0" $ do
            let r = RetrySettings 3 1000 0.5
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `textShouldContain` "retryBackoffMultiplier"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects negative retryMaxAttempts" $ do
            let r = RetrySettings (-1) 1000 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `textShouldContain` "retryMaxAttempts"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects zero retryInitialDelayMs" $ do
            let r = RetrySettings 3 0 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `textShouldContain` "retryInitialDelayMs"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts valid retry settings" $ do
            let r = RetrySettings 3 1000 2.0
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {retry = Just r}}
            validateConfig cfg `shouldBe` Right cfg
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
    , testGroup
        "loadMode JSON parsing"
        [ testCase "parses constantRps" $ do
            let json = "{\"mode\": \"constantRps\", \"targetRps\": 50.0}"
            case eitherDecode json :: Either String LoadMode of
              Right (LoadConstantRps rps) -> rps `shouldBe` 50.0
              other -> assertFailure $ "Expected LoadConstantRps, got: " ++ show other
        , testCase "parses rampUp" $ do
            let json = "{\"mode\": \"rampUp\", \"startRps\": 10.0, \"endRps\": 100.0, \"durationSecs\": 60.0}"
            case eitherDecode json :: Either String LoadMode of
              Right (LoadRampUp s e d) -> do
                s `shouldBe` 10.0
                e `shouldBe` 100.0
                d `shouldBe` 60.0
              other -> assertFailure $ "Expected LoadRampUp, got: " ++ show other
        , testCase "parses stepLoad" $ do
            let json =
                  "{\"mode\": \"stepLoad\", \"steps\": [{\"rps\": 20.0, \"durationSecs\": 30.0}, {\"rps\": 50.0, \"durationSecs\": 30.0}]}"
            case eitherDecode json :: Either String LoadMode of
              Right (LoadStepLoad steps) -> length steps `shouldBe` 2
              other -> assertFailure $ "Expected LoadStepLoad, got: " ++ show other
        , testCase "parses unthrottled" $ do
            let json = "{\"mode\": \"unthrottled\"}"
            eitherDecode json `shouldBe` Right LoadUnthrottled
        , testCase "missing loadMode defaults to Nothing" $ do
            -- makeValidConfig has loadMode = Nothing
            let cfg = makeValidConfig
            loadMode (settings cfg) `shouldBe` Nothing
        , testCase "rejects unknown mode" $ do
            let json = "{\"mode\": \"turbo\"}"
            case eitherDecode json :: Either String LoadMode of
              Left _ -> pure ()
              Right _ -> assertFailure "Expected parse failure for unknown mode"
        ]
    , testGroup
        "loadMode validation"
        [ testCase "accepts valid constantRps" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadConstantRps 50)}}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "rejects constantRps with rps=0" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadConstantRps 0)}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "targetRps"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects rampUp with startRps=0" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadRampUp 0 100 60)}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "startRps"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects rampUp with negative durationSecs" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadRampUp 10 100 (-1))}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "durationSecs"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects stepLoad with empty steps" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadStepLoad [])}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "steps must not be empty"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects stepLoad with zero rps" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadStepLoad [LoadStep 0 30])}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "step rps"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects stepLoad with zero durationSecs" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadStepLoad [LoadStep 50 0])}}
            case validateConfig cfg of
              Left (ConfigValidationError msg) -> msg `textShouldContain` "step durationSecs"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts valid rampUp" $ do
            let cfg = makeValidConfig {settings = (settings makeValidConfig) {loadMode = Just (LoadRampUp 10 100 60)}}
            validateConfig cfg `shouldBe` Right cfg
        , testCase "accepts valid stepLoad" $ do
            let cfg =
                  makeValidConfig
                    { settings = (settings makeValidConfig) {loadMode = Just (LoadStepLoad [LoadStep 20 30, LoadStep 50 30])}
                    }
            validateConfig cfg `shouldBe` Right cfg
        , testCase "accepts Nothing loadMode (backwards compat)" $ do
            validateConfig makeValidConfig `shouldBe` Right makeValidConfig
        ]
    ]
