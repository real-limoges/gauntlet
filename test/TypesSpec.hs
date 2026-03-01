module TypesSpec (typesSpec) where

import Benchmark.Environment (trim)
import Benchmark.Types
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

typesSpec :: TestTree
typesSpec =
  testGroup
    "TypesSpec"
    [ testGroup
        "Benchmark.Types"
        [ testGroup
            "nsToMs"
            [ testCase "converts 1_000_000 ns to 1.0 ms" $ do
                nsToMs (Nanoseconds 1_000_000) `shouldBe` Milliseconds 1.0
            ]
        , testGroup
            "formatError"
            [ testCase "formats ConfigParseError" $ do
                formatError (ConfigParseError "bad json") `shouldBe` "Failed to parse config: bad json"
            , testCase "formats ConfigValidationError" $ do
                formatError (ConfigValidationError "missing field") `shouldBe` "Invalid config: missing field"
            , testCase "formats TokenReadError with path and message" $ do
                formatError (TokenReadError "/secrets.txt" "permission denied")
                  `shouldBe` "Failed to read token from /secrets.txt: permission denied"
            , testCase "formats HealthCheckTimeout with URL and retry count" $ do
                formatError (HealthCheckTimeout "http://svc:8080/health" 60)
                  `shouldBe` "Service at http://svc:8080/health failed to start after 60 retries"
            , testCase "formats NoEndpointsError" $ do
                formatError (NoEndpointsError "primary") `shouldBe` "No primary endpoints defined"
            , testCase "formats EnvironmentSetupError" $ do
                formatError (EnvironmentSetupError "git checkout failed")
                  `shouldBe` "Environment setup failed: git checkout failed"
            ]
        ]
    , testGroup
        "Benchmark.Environment.trim"
        [ testCase "strips leading and trailing spaces" $ do
            trim "  hello  " `shouldBe` "hello"
        , testCase "strips tabs and newlines" $ do
            trim "\n\tfoo\r\n" `shouldBe` "foo"
        ]
    ]
