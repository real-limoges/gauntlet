-- | Tests for Benchmark.Types.
module TypesSpec (typesSpec) where

import Benchmark.Types (PerfTestError (..), formatError)
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
                formatError (ConfigParseError "bad json") `shouldBe` "Config parse error: bad json"
            , testCase "formats ConfigValidationError" $ do
                formatError (ConfigValidationError "missing field") `shouldBe` "Config validation error: missing field"
            , testCase "formats TokenReadError with path and message" $ do
                formatError (TokenReadError "/secrets.txt" "permission denied")
                  `shouldBe` "Token read error (/secrets.txt): permission denied"
            , testCase "formats HealthCheckTimeout with URL and retry count" $ do
                formatError (HealthCheckTimeout "http://svc:8080/health" 60)
                  `shouldBe` "Health check timeout: http://svc:8080/health failed after 60 retries"
            , testCase "formats NoEndpointsError" $ do
                formatError (NoEndpointsError "primary") `shouldBe` "No primary endpoints defined"
            , testCase "formats GitSwitchError" $ do
                formatError (GitSwitchError "branch not found")
                  `shouldBe` "Git switch error: branch not found"
            , testCase "formats HookSetupError" $ do
                formatError (HookSetupError "docker-compose failed")
                  `shouldBe` "Setup hook failed: docker-compose failed"
            , testCase "formats HookTeardownError" $ do
                formatError (HookTeardownError "docker-compose down failed")
                  `shouldBe` "Teardown hook failed: docker-compose down failed"
            , testCase "formats HookTimeoutError" $ do
                formatError (HookTimeoutError "docker-compose up" 120)
                  `shouldBe` "Hook timed out after 120s: docker-compose up"
            , testCase "formats BenchmarkCancelled" $ do
                formatError BenchmarkCancelled `shouldBe` "Benchmark cancelled by user"
            , testCase "formats NetworkTimeout" $ do
                formatError (NetworkTimeout "http://svc:8080")
                  `shouldBe` "Network timeout: http://svc:8080"
            , testCase "formats ConnectionRefused" $ do
                formatError (ConnectionRefused "http://svc:8080")
                  `shouldBe` "Connection refused: http://svc:8080"
            , testCase "formats TlsError" $ do
                formatError (TlsError "https://svc:443" "handshake failed")
                  `shouldBe` "TLS error (https://svc:443): handshake failed"
            , testCase "formats HttpError" $ do
                formatError (HttpError "http://svc:8080" 503)
                  `shouldBe` "HTTP 503 from http://svc:8080"
            , testCase "formats UnknownNetworkError" $ do
                formatError (UnknownNetworkError "something went wrong")
                  `shouldBe` "Network error: something went wrong"
            ]
        ]
    ]
