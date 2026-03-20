-- | Tests for Benchmark.Execution.Environment.
module EnvironmentSpec (environmentSpec) where

import Benchmark.Execution.Environment (runHook, waitForHealth)
import Benchmark.Types (PerfTestError (..))
import Benchmark.Types (HealthCheckConfig (..), HookCommand (..))
import Data.Text qualified as T
import MockServer (mockFailThenSucceed, mockStatus)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200, status503)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

environmentSpec :: TestTree
environmentSpec =
  testGroup
    "Benchmark.Environment"
    [ testGroup
        "waitForHealth"
        [ testCase "timeoutSecs = 0 immediately returns HealthCheckTimeout" $ do
            mgr <- newManager tlsManagerSettings
            let hc = HealthCheckConfig "http://127.0.0.1:99999/health" (Just 0) Nothing
            result <- waitForHealth mgr hc
            case result of
              Left (HealthCheckTimeout _ 0) -> pure ()
              other -> assertFailure $ "Expected HealthCheckTimeout with 0 retries, got: " ++ show other
        , testCase "returns Right () when server returns 200" $
            mockStatus status200 $ \port -> do
              mgr <- newManager tlsManagerSettings
              let url = T.pack $ "http://127.0.0.1:" ++ show port
              let hc = HealthCheckConfig url (Just 5) Nothing
              result <- waitForHealth mgr hc
              result `shouldBe` Right ()
        , testCase "returns HealthCheckTimeout when server always returns 503 with timeoutSecs = 1" $
            mockStatus status503 $ \port -> do
              mgr <- newManager tlsManagerSettings
              let url = T.pack $ "http://127.0.0.1:" ++ show port
              let hc = HealthCheckConfig url (Just 1) Nothing
              result <- waitForHealth mgr hc
              case result of
                Left (HealthCheckTimeout _ _) -> pure ()
                other -> assertFailure $ "Expected HealthCheckTimeout, got: " ++ show other
        , testCase "succeeds when server returns 503 then 200" $
            mockFailThenSucceed 1 $ \port -> do
              mgr <- newManager tlsManagerSettings
              let url = T.pack $ "http://127.0.0.1:" ++ show port
              let hc = HealthCheckConfig url (Just 5) Nothing
              result <- waitForHealth mgr hc
              result `shouldBe` Right ()
        , testCase "times out when all retries see 503" $
            mockStatus status503 $ \port -> do
              mgr <- newManager tlsManagerSettings
              let url = T.pack $ "http://127.0.0.1:" ++ show port
              let hc = HealthCheckConfig url (Just 2) Nothing
              result <- waitForHealth mgr hc
              case result of
                Left (HealthCheckTimeout _ 2) -> pure ()
                other -> assertFailure $ "Expected HealthCheckTimeout with 2 retries, got: " ++ show other
        ]
    , testGroup
        "runHook"
        [ testCase "echo command succeeds and returns Right ()" $ do
            let cmd = HookCommand "echo hello" Nothing Nothing
            result <- runHook cmd HookSetupError
            result `shouldBe` Right ()
        , testCase "failing command returns Left HookSetupError" $ do
            let cmd = HookCommand "exit 1" Nothing Nothing
            result <- runHook cmd HookSetupError
            case result of
              Left (HookSetupError _) -> pure ()
              other -> assertFailure $ "Expected HookSetupError, got: " ++ show other
        , testCase "nonexistent command returns Left error" $ do
            let cmd = HookCommand "this-command-does-not-exist-xyz" Nothing Nothing
            result <- runHook cmd HookSetupError
            case result of
              Left (HookSetupError _) -> pure ()
              other -> assertFailure $ "Expected HookSetupError, got: " ++ show other
        ]
    ]
