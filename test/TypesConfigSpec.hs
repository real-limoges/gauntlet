-- | Tests for Benchmark.Types.Config.
module TypesConfigSpec (typesConfigSpec) where

import Benchmark.Types
import Data.Aeson (decode, eitherDecode, encode)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

typesConfigSpec :: TestTree
typesConfigSpec =
  testGroup
    "Benchmark.Types.Config"
    [ testGroup
        "totalRequestsForMode"
        [ testCase "LoadUnthrottled returns fallback" $
            totalRequestsForMode LoadUnthrottled 100 `shouldBe` 100
        , testCase "LoadConstantRpm returns fallback" $
            totalRequestsForMode (LoadConstantRpm 10) 100 `shouldBe` 100
        , testCase "LoadPoissonRpm returns fallback" $
            totalRequestsForMode (LoadPoissonRpm 5) 100 `shouldBe` 100
        , testCase "LoadRampUp (RampUpConfig 600 1200 5) returns 75" $
            -- (600+1200)/2/60*5 = 75
            totalRequestsForMode (LoadRampUp (RampUpConfig 600 1200 5)) 0 `shouldBe` 75
        , testCase "LoadStepLoad sums rpm/60*duration per step" $
            let steps =
                  [ LoadStep {loadStepRpm = 600, loadStepDurationSecs = 5}
                  , LoadStep {loadStepRpm = 1200, loadStepDurationSecs = 3}
                  ]
             in totalRequestsForMode (LoadStepLoad steps) 0 `shouldBe` 110
        ]
    , testGroup
        "isDurationBased"
        [ testCase "LoadRampUp is True" $
            isDurationBased (LoadRampUp (RampUpConfig 10 20 5)) `shouldBe` True
        , testCase "LoadStepLoad is True" $
            isDurationBased (LoadStepLoad []) `shouldBe` True
        , testCase "LoadUnthrottled is False" $
            isDurationBased LoadUnthrottled `shouldBe` False
        , testCase "LoadConstantRpm is False" $
            isDurationBased (LoadConstantRpm 10) `shouldBe` False
        , testCase "LoadPoissonRpm is False" $
            isDurationBased (LoadPoissonRpm 5) `shouldBe` False
        ]
    , testGroup
        "loadModeDurationSecs"
        [ testCase "LoadRampUp returns its duration" $
            loadModeDurationSecs (LoadRampUp (RampUpConfig 10 20 30)) `shouldBe` 30.0
        , testCase "LoadStepLoad sums step durations" $
            loadModeDurationSecs
              ( LoadStepLoad
                  [ LoadStep {loadStepRpm = 10, loadStepDurationSecs = 5}
                  , LoadStep {loadStepRpm = 20, loadStepDurationSecs = 10}
                  ]
              )
              `shouldBe` 15.0
        , testCase "LoadUnthrottled returns 0" $
            loadModeDurationSecs LoadUnthrottled `shouldBe` 0.0
        , testCase "LoadConstantRpm returns 0" $
            loadModeDurationSecs (LoadConstantRpm 10) `shouldBe` 0.0
        , testCase "LoadPoissonRpm returns 0" $
            loadModeDurationSecs (LoadPoissonRpm 5) `shouldBe` 0.0
        ]
    , testGroup
        "LoadStep JSON round-trip"
        [ testCase "encode then decode returns same value" $
            let step = LoadStep {loadStepRpm = 42.5, loadStepDurationSecs = 10.0}
             in decode (encode step) `shouldBe` Just step
        ]
    , testGroup
        "default values"
        [ testCase "defaultRetrySettings.retryMaxAttempts == 3" $
            retryMaxAttempts defaultRetrySettings `shouldBe` 3
        , testCase "defaultRetrySettings.retryInitialDelayMs == 1000" $
            retryInitialDelayMs defaultRetrySettings `shouldBe` 1000
        , testCase "defaultRetrySettings.retryBackoffMultiplier == 2.0" $
            retryBackoffMultiplier defaultRetrySettings `shouldBe` 2.0
        , testCase "defaultWarmupSettings.warmupIterations == 1" $
            warmupIterations defaultWarmupSettings `shouldBe` 1
        , testCase "defaultLogLevel == Info" $
            defaultLogLevel `shouldBe` Info
        ]
    , testGroup
        "LifecycleHooks JSON"
        [ testCase "parses setup hook from JSON" $ do
            let json = "{\"setup\": {\"cmd\": \"docker-compose up -d\", \"timeoutSecs\": 120}}"
            case eitherDecode json :: Either String LifecycleHooks of
              Right hooks -> case hookSetup hooks of
                Just cmd -> hookCmd cmd `shouldBe` "docker-compose up -d"
                Nothing -> assertFailure "Expected hookSetup to be set"
              Left err -> assertFailure $ "Parse failed: " ++ err
        , testCase "parses healthCheck from JSON" $ do
            let json = "{\"healthCheck\": {\"url\": \"http://localhost:8080/health\", \"timeoutSecs\": 60, \"intervalMs\": 500}}"
            case eitherDecode json :: Either String LifecycleHooks of
              Right hooks -> case hookHealthCheck hooks of
                Just hc -> do
                  hcUrl hc `shouldBe` "http://localhost:8080/health"
                  hcTimeoutSecs hc `shouldBe` Just 60
                  hcIntervalMs hc `shouldBe` Just 500
                Nothing -> assertFailure "Expected hookHealthCheck to be set"
              Left err -> assertFailure $ "Parse failed: " ++ err
        , testCase "all fields absent parses as all Nothing" $ do
            let json = "{}"
            case eitherDecode json :: Either String LifecycleHooks of
              Right hooks -> do
                hookSetup hooks `shouldBe` Nothing
                hookTeardown hooks `shouldBe` Nothing
                hookHealthCheck hooks `shouldBe` Nothing
              Left err -> assertFailure $ "Parse failed: " ++ err
        ]
    , testGroup
        "HookCommand JSON"
        [ testCase "parses cmd field" $ do
            let json = "{\"cmd\": \"echo hello\"}"
            case eitherDecode json :: Either String HookCommand of
              Right cmd -> hookCmd cmd `shouldBe` "echo hello"
              Left err -> assertFailure $ "Parse failed: " ++ err
        , testCase "optional fields default to Nothing" $ do
            let json = "{\"cmd\": \"echo hi\"}"
            case eitherDecode json :: Either String HookCommand of
              Right cmd -> do
                hookTimeoutSecs cmd `shouldBe` Nothing
                hookWorkingDir cmd `shouldBe` Nothing
              Left err -> assertFailure $ "Parse failed: " ++ err
        ]
    , testGroup
        "HealthCheckConfig JSON"
        [ testCase "parses url field" $ do
            let json = "{\"url\": \"http://localhost:8080/health\"}"
            case eitherDecode json :: Either String HealthCheckConfig of
              Right hc -> hcUrl hc `shouldBe` "http://localhost:8080/health"
              Left err -> assertFailure $ "Parse failed: " ++ err
        , testCase "optional fields default to Nothing" $ do
            let json = "{\"url\": \"http://localhost/health\"}"
            case eitherDecode json :: Either String HealthCheckConfig of
              Right hc -> do
                hcTimeoutSecs hc `shouldBe` Nothing
                hcIntervalMs hc `shouldBe` Nothing
              Left err -> assertFailure $ "Parse failed: " ++ err
        ]
    , testGroup
        "NamedTarget with lifecycle JSON"
        [ testCase "parses target with lifecycle hooks" $ do
            let json =
                  "{\"name\": \"local\", \"url\": \"http://localhost:8080\", \
                  \\"lifecycle\": {\"setup\": {\"cmd\": \"docker-compose up -d\"}, \
                  \\"healthCheck\": {\"url\": \"http://localhost:8080/health\"}}}"
            case eitherDecode json :: Either String NamedTarget of
              Right t -> do
                targetName t `shouldBe` "local"
                case targetLifecycle t of
                  Just hooks -> case hookSetup hooks of
                    Just cmd -> hookCmd cmd `shouldBe` "docker-compose up -d"
                    Nothing -> assertFailure "Expected hookSetup"
                  Nothing -> assertFailure "Expected lifecycle"
              Left err -> assertFailure $ "Parse failed: " ++ err
        , testCase "parses target without lifecycle" $ do
            let json = "{\"name\": \"prod\", \"url\": \"http://prod.example.com\"}"
            case eitherDecode json :: Either String NamedTarget of
              Right t -> targetLifecycle t `shouldBe` Nothing
              Left err -> assertFailure $ "Parse failed: " ++ err
        ]
    ]
