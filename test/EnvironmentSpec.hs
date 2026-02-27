module EnvironmentSpec (environmentSpec) where

import Benchmark.Environment (waitForHealth)
import Benchmark.Types (PerfTestError (..))
import Data.Text qualified as T
import MockServer (mockFailThenSucceed, mockStatus)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200, status503)
import Test.Hspec

environmentSpec :: Spec
environmentSpec = describe "Benchmark.Environment.waitForHealth" $ do
  it "maxRetries = 0 immediately returns HealthCheckTimeout" $ do
    mgr <- newManager tlsManagerSettings
    result <- waitForHealth mgr "http://127.0.0.1:99999/health" 0
    case result of
      Left (HealthCheckTimeout _ 0) -> pure ()
      other -> expectationFailure $ "Expected HealthCheckTimeout with 0 retries, got: " ++ show other

  it "returns Right () when server returns 200" $
    mockStatus status200 $ \port -> do
      mgr <- newManager tlsManagerSettings
      let url = T.pack $ "http://127.0.0.1:" ++ show port
      result <- waitForHealth mgr url 5
      result `shouldBe` Right ()

  it "returns HealthCheckTimeout when server always returns 503 with maxRetries = 1" $
    mockStatus status503 $ \port -> do
      mgr <- newManager tlsManagerSettings
      let url = T.pack $ "http://127.0.0.1:" ++ show port
      result <- waitForHealth mgr url 1
      case result of
        Left (HealthCheckTimeout _ _) -> pure ()
        other -> expectationFailure $ "Expected HealthCheckTimeout, got: " ++ show other

  it "returns Right () on healthy server even with high maxRetries" $
    mockStatus status200 $ \port -> do
      mgr <- newManager tlsManagerSettings
      let url = T.pack $ "http://127.0.0.1:" ++ show port
      result <- waitForHealth mgr url 10
      result `shouldBe` Right ()

  it "succeeds when server returns 503 then 200" $
    mockFailThenSucceed 1 $ \port -> do
      mgr <- newManager tlsManagerSettings
      let url = T.pack $ "http://127.0.0.1:" ++ show port
      result <- waitForHealth mgr url 5
      result `shouldBe` Right ()

  it "times out when all retries see 503" $
    mockStatus status503 $ \port -> do
      mgr <- newManager tlsManagerSettings
      let url = T.pack $ "http://127.0.0.1:" ++ show port
      result <- waitForHealth mgr url 2
      case result of
        Left (HealthCheckTimeout _ 2) -> pure ()
        other -> expectationFailure $ "Expected HealthCheckTimeout with 2 retries, got: " ++ show other
