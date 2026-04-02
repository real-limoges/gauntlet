-- | Tests for Benchmark.Network.Exec.
module ExecSpec (execSpec) where

import Benchmark.Network.Exec (BenchmarkEnv (..), runBenchmark)
import Benchmark.Network.Request (initNetwork)
import Benchmark.TUI.State (BenchmarkEvent)
import Benchmark.Types
  ( Endpoint (..)
  , RetrySettings (..)
  , Settings (..)
  , TestingResponse (..)
  , defaultLogLevel
  )
import Control.Concurrent (newQSem)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue)
import Control.Monad (replicateM)
import Data.Text qualified as T
import Log (makeLogger)
import MockServer (mockCountedRequests, mockJson, mockStatus)
import Network.HTTP.Types (status200, status500)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

execSpec :: TestTree
execSpec =
  testGroup
    "Benchmark.Network.Exec"
    [ testCase "runBenchmark with events emits one event per iteration" $ do
        chan <- newTBQueueIO 100
        mockJson "{}" $ \port -> do
          mgr <- initNetwork testSettings
          sem <- newQSem 1
          let env = BenchmarkEnv testSettings sem mgr 1 (Just (chan :: TBQueue BenchmarkEvent)) (makeLogger defaultLogLevel)
          results <- runBenchmark env 5 (testEndpoint port) Nothing
          length results `shouldBe` 5
          events <- replicateM 5 (atomically (readTBQueue chan))
          length events `shouldBe` 5
    , testCase "all-500 server returns responses with status 500" $ do
        mockStatus status500 $ \port -> do
          mgr <- initNetwork testSettings
          sem <- newQSem 1
          let env = BenchmarkEnv testSettings sem mgr 1 Nothing (makeLogger defaultLogLevel)
          results <- runBenchmark env 5 (testEndpoint port) Nothing
          length results `shouldBe` 5
          all (\r -> statusCode r == 500) results `shouldBe` True
    , testCase "concurrency: all 10 requests complete" $
        mockCountedRequests status200 "{}" $ \port readCount -> do
          mgr <- initNetwork testSettings
          sem <- newQSem 4
          let env = BenchmarkEnv testSettings sem mgr 1 Nothing (makeLogger defaultLogLevel)
          results <- runBenchmark env 10 (testEndpoint port) Nothing
          length results `shouldBe` 10
          count <- readCount
          count `shouldBe` 10
    ]

testSettings :: Settings
testSettings =
  Settings
    { iterations = 5
    , concurrency = 1
    , secrets = Nothing
    , maxConnections = Just 5
    , requestTimeout = Just 10
    , retry = Just (RetrySettings 0 0 1.0)
    , warmup = Nothing
    , logLevel = Nothing
    , tempo = Nothing
    , loadMode = Nothing
    }

testEndpoint :: Int -> Endpoint
testEndpoint port =
  Endpoint
    { method = "GET"
    , url = "http://127.0.0.1:" <> T.pack (show port)
    , body = Nothing
    , headers = []
    , validate = Nothing
    }
