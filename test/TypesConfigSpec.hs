-- | Tests for Benchmark.Types.Config.
module TypesConfigSpec (typesConfigSpec) where

import Benchmark.Types
import Data.Aeson (decode, encode)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

typesConfigSpec :: TestTree
typesConfigSpec =
  testGroup
    "Benchmark.Types.Config"
    [ testGroup
        "totalRequestsForMode"
        [ testCase "LoadUnthrottled returns fallback" $
            totalRequestsForMode LoadUnthrottled 100 `shouldBe` 100
        , testCase "LoadConstantRps returns fallback" $
            totalRequestsForMode (LoadConstantRps 10) 100 `shouldBe` 100
        , testCase "LoadPoissonRps returns fallback" $
            totalRequestsForMode (LoadPoissonRps 5) 100 `shouldBe` 100
        , testCase "LoadRampUp (RampUpConfig 10 20 5) returns 75" $
            totalRequestsForMode (LoadRampUp (RampUpConfig 10 20 5)) 0 `shouldBe` 75
        , testCase "LoadStepLoad sums rps*duration per step" $
            let steps =
                  [LoadStep {loadStepRps = 10, loadStepDurationSecs = 5}, LoadStep {loadStepRps = 20, loadStepDurationSecs = 3}]
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
        , testCase "LoadConstantRps is False" $
            isDurationBased (LoadConstantRps 10) `shouldBe` False
        , testCase "LoadPoissonRps is False" $
            isDurationBased (LoadPoissonRps 5) `shouldBe` False
        ]
    , testGroup
        "loadModeDurationSecs"
        [ testCase "LoadRampUp returns its duration" $
            loadModeDurationSecs (LoadRampUp (RampUpConfig 10 20 30)) `shouldBe` 30.0
        , testCase "LoadStepLoad sums step durations" $
            loadModeDurationSecs
              ( LoadStepLoad
                  [ LoadStep {loadStepRps = 10, loadStepDurationSecs = 5}
                  , LoadStep {loadStepRps = 20, loadStepDurationSecs = 10}
                  ]
              )
              `shouldBe` 15.0
        , testCase "LoadUnthrottled returns 0" $
            loadModeDurationSecs LoadUnthrottled `shouldBe` 0.0
        , testCase "LoadConstantRps returns 0" $
            loadModeDurationSecs (LoadConstantRps 10) `shouldBe` 0.0
        , testCase "LoadPoissonRps returns 0" $
            loadModeDurationSecs (LoadPoissonRps 5) `shouldBe` 0.0
        ]
    , testGroup
        "LoadStep JSON round-trip"
        [ testCase "encode then decode returns same value" $
            let step = LoadStep {loadStepRps = 42.5, loadStepDurationSecs = 10.0}
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
    ]
