module NwaySpec (nwaySpec) where

import Benchmark.Config.Loader (validateNwayConfig)
import Benchmark.Types
import Data.Text (Text)
import Data.Text qualified as T
import Runner.Nway (allPairComparisons)
import TastyCompat (shouldBe, shouldContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers (mockStats)

nwaySpec :: TestTree
nwaySpec =
  testGroup
    "N-Way Benchmarking"
    [ testGroup
        "validateNwayConfig"
        [ testCase "rejects configs with fewer than 2 targets" $ do
            let cfg = makeNwayConfig [NamedTarget "a" "http://a" Nothing]
            case validateNwayConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldBe` "Must have at least 2 targets"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects empty payloads" $ do
            let cfg =
                  NwayConfig
                    { nwayTargets =
                        [ NamedTarget "a" "http://a" Nothing
                        , NamedTarget "b" "http://b" Nothing
                        ]
                    , nwaySettings = defaultSettings
                    , nwayPayloads = []
                    }
            case validateNwayConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldBe` "No payloads defined in config"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "rejects invalid HTTP methods" $ do
            let cfg =
                  NwayConfig
                    { nwayTargets =
                        [ NamedTarget "a" "http://a" Nothing
                        , NamedTarget "b" "http://b" Nothing
                        ]
                    , nwaySettings = defaultSettings
                    , nwayPayloads = [PayloadSpec "test" "INVALID" "/path" Nothing Nothing Nothing]
                    }
            case validateNwayConfig cfg of
              Left (ConfigValidationError msg) ->
                msg `shouldContain` "Invalid HTTP method"
              _ -> assertFailure "Expected ConfigValidationError"
        , testCase "accepts valid 2-target config" $ do
            let cfg = makeNwayConfig [NamedTarget "a" "http://a" Nothing, NamedTarget "b" "http://b" Nothing]
            validateNwayConfig cfg `shouldBe` Right cfg
        ]
    , testGroup
        "allPairComparisons"
        [ testCase "produces 1 pair for 2 targets" $ do
            let stats = makePairStats 2
            length (allPairComparisons stats) `shouldBe` 1
        , testCase "produces 3 pairs for 3 targets" $ do
            let stats = makePairStats 3
            length (allPairComparisons stats) `shouldBe` 3
        , testCase "produces 0 pairs for empty list" $ do
            allPairComparisons [] `shouldBe` []
        , testCase "produces 0 pairs for single target" $ do
            allPairComparisons [("only", mockStats 10 1)] `shouldBe` []
        , testCase "includes correct target names in pairs" $ do
            let stats =
                  [ ("alpha", mockStats 10 1)
                  , ("beta", mockStats 20 2)
                  , ("gamma", mockStats 30 3)
                  ]
            let pairs = allPairComparisons stats
            let pairNames = [(a, b) | (a, b, _) <- pairs]
            pairNames `shouldBe` [("alpha", "beta"), ("alpha", "gamma"), ("beta", "gamma")]
        ]
    ]

-- Helpers

defaultSettings :: Settings
defaultSettings =
  Settings
    { iterations = 10
    , concurrency = 2
    , secrets = Just "secrets.txt"
    , maxConnections = Nothing
    , requestTimeout = Nothing
    , retry = Nothing
    , warmup = Nothing
    , logLevel = Nothing
    , tempo = Nothing
    , healthCheckPath = Nothing
    , healthCheckTimeout = Nothing
    , loadMode = Nothing
    }

makeNwayConfig :: [NamedTarget] -> NwayConfig
makeNwayConfig ts =
  NwayConfig
    { nwayTargets = ts
    , nwaySettings = defaultSettings
    , nwayPayloads = [PayloadSpec "test" "GET" "/api/test" Nothing Nothing Nothing]
    }

makePairStats :: Int -> [(Text, BenchmarkStats)]
makePairStats n =
  [ (T.pack ("target-" ++ show i), mockStats (fromIntegral i * 10) (fromIntegral i))
  | i <- [1 .. n]
  ]
