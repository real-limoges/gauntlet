-- | Integration tests for load control modes.
module LoadControlIntegrationSpec (loadControlIntegrationSpec) where

import Benchmark.Execution.RateLimiter (makeLimiter)
import Benchmark.Network (BenchmarkEnv (..), runBenchmark, runBenchmarkDuration)
import Benchmark.Types
  ( Endpoint (..)
  , LoadMode (..)
  , RampUpConfig (..)
  , RetrySettings (..)
  , Settings (..)
  , TestConfig (..)
  , TestingResponse (..)
  , defaultLogLevel
  )
import Control.Concurrent (newQSem)
import Log (makeLogger)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import MockServer (mockJson)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (makeValidConfig)

loadControlIntegrationSpec :: TestTree
loadControlIntegrationSpec =
  testGroup
    "LoadControl Integration"
    [ testCase "constant RPS: completes expected iterations in expected time" $ do
        mgr <- newManager tlsManagerSettings
        mockJson "{}" $ \port -> do
          -- 10 requests at 20 RPS => ~0.45s (first fires immediately, then 9 intervals of 50ms)
          Just limiter <- makeLimiter (LoadConstantRps 20)
          sem <- newQSem 4
          start <- getCurrentTime
          let env = BenchmarkEnv testSettings sem mgr 1 Nothing (makeLogger defaultLogLevel)
          results <- runBenchmark env 10 (endpoint port) (Just limiter)
          end <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime end start) :: Double
          length results `shouldBe` 10
          all ((== 200) . statusCode) results `shouldBe` True
          -- 9 intervals at 50ms = 450ms min, allow generous upper bound
          elapsed `shouldSatisfy` (>= 0.35)
          elapsed `shouldSatisfy` (< 2.0)
    , testCase "duration-based: runs for specified duration" $ do
        mgr <- newManager tlsManagerSettings
        mockJson "{}" $ \port -> do
          -- Run at 10 RPS for 1 second => ~10 requests
          Just limiter <- makeLimiter (LoadConstantRps 10)
          sem <- newQSem 4
          start <- getCurrentTime
          let env = BenchmarkEnv durationSettings sem mgr 1 Nothing (makeLogger defaultLogLevel)
          results <- runBenchmarkDuration env 1.0 (endpoint port) limiter
          end <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime end start) :: Double
          -- Should get some requests (at least 4, at most ~20)
          length results `shouldSatisfy` (>= 4)
          length results `shouldSatisfy` (<= 20)
          -- Should complete in roughly 1 second
          elapsed `shouldSatisfy` (>= 0.8)
          elapsed `shouldSatisfy` (< 3.0)
    , testCase "no loadMode: runs without pacing" $ do
        mgr <- newManager tlsManagerSettings
        mockJson "{}" $ \port -> do
          sem <- newQSem 4
          let env = BenchmarkEnv testSettings sem mgr 1 Nothing (makeLogger defaultLogLevel)
          results <- runBenchmark env 5 (endpoint port) Nothing
          length results `shouldBe` 5
          all ((== 200) . statusCode) results `shouldBe` True
    ]

testSettings :: Settings
testSettings =
  (settings (makeValidConfig :: TestConfig))
    { iterations = 10
    , concurrency = 4
    , secrets = Nothing
    , retry = Just (RetrySettings 0 100 1.0)
    , requestTimeout = Just 5
    }

durationSettings :: Settings
durationSettings = testSettings

endpoint :: Int -> Endpoint
endpoint port =
  Endpoint
    { method = "GET"
    , url = "http://127.0.0.1:" <> T.pack (show port)
    , body = Nothing
    , headers = []
    , validate = Nothing
    }
