-- | Tests for Runner.Loop.
module LoopSpec (loopSpec) where

import Benchmark.Types
import Control.Exception (try)
import Data.Text qualified as T
import MockServer (mockJson)
import Runner.Context (RunContext, initContext)
import Runner.Loop (benchmarkEndpoints)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

loopSpec :: TestTree
loopSpec =
  testGroup
    "Runner.Loop"
    [ testCase "empty endpoints throws NoEndpointsError" $
        withCtx $ \ctx -> do
          result <-
            ( try (benchmarkEndpoints ctx "test" []) ::
                IO (Either PerfTestError ([TestingResponse], [ValidationSummary]))
            )
          result `shouldBe` Left (NoEndpointsError "test")
    , testCase "single endpoint returns 10 responses, no validation summaries" $
        mockJson "{}" $ \port ->
          withCtx $ \ctx -> do
            (responses, summaries) <- benchmarkEndpoints ctx "test" [testEndpoint port]
            length responses `shouldBe` 10
            null summaries `shouldBe` True
    , testCase "endpoint with validate produces one validation summary" $
        mockJson "{}" $ \port ->
          withCtx $ \ctx -> do
            let ep =
                  (testEndpoint port)
                    { validate = Just ValidationSpec {validateStatus = Just 200, validateFields = Nothing}
                    }
            (_, summaries) <- benchmarkEndpoints ctx "test" [ep]
            length summaries `shouldBe` 1
    ]

testSettings :: Settings
testSettings =
  Settings
    { iterations = 10
    , concurrency = 2
    , secrets = Nothing
    , maxConnections = Just 5
    , requestTimeout = Just 10
    , retry = Just (RetrySettings 0 0 1.0)
    , warmup = Just WarmupSettings {warmupIterations = 0}
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

withCtx :: (RunContext -> IO a) -> IO a
withCtx action =
  withSystemTempFile "loop-test.csv" $ \csvPath h -> do
    hClose h
    ctx <- initContext testSettings csvPath "ts" Nothing
    action ctx
