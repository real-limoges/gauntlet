module TypesSpec (typesSpec) where

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
            , testCase "formats BenchmarkCancelled" $ do
                formatError BenchmarkCancelled `shouldBe` "Benchmark cancelled by user"
            , testCase "formats NetworkTimeout" $ do
                formatError (NetworkTimeout "http://svc:8080")
                  `shouldBe` "Request to http://svc:8080 timed out"
            , testCase "formats ConnectionRefused" $ do
                formatError (ConnectionRefused "http://svc:8080")
                  `shouldBe` "Connection refused: http://svc:8080"
            , testCase "formats TlsError" $ do
                formatError (TlsError "https://svc:443" "handshake failed")
                  `shouldBe` "TLS error connecting to https://svc:443: handshake failed"
            , testCase "formats HttpError" $ do
                formatError (HttpError "http://svc:8080" 503)
                  `shouldBe` "HTTP 503 from http://svc:8080"
            , testCase "formats UnknownNetworkError" $ do
                formatError (UnknownNetworkError "something went wrong")
                  `shouldBe` "Network error: something went wrong"
            ]
        ]
    ]
