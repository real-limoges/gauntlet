-- | Tests for Runner.Warmup.
module WarmupSpec (warmupSpec) where

import Benchmark.Types
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import MockServer (mockCountedRequests)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200)
import Runner.Context (RunContext (..))
import Runner.Warmup (runWarmup)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (makeCapturingLogger, makeValidConfig)

warmupSpec :: TestTree
warmupSpec =
  testGroup
    "Runner.Warmup"
    [ testGroup
        "runWarmup"
        [ testCase "warmupIterations = 0 makes no requests and emits no logs" $
            mockCountedRequests status200 "{}" $ \port getCount -> do
              (ctx, logRef) <- makeCtxWithWarmupAndLog port 0
              let ep = makeEndpoint port
              runWarmup ctx ep
              count <- getCount
              count `shouldBe` 0
              msgs <- readIORef logRef
              msgs `shouldBe` []
        , testCase "warmupIterations = 1 makes 1 request and logs singular 'iteration'" $
            mockCountedRequests status200 "{}" $ \port getCount -> do
              (ctx, logRef) <- makeCtxWithWarmupAndLog port 1
              let ep = makeEndpoint port
              runWarmup ctx ep
              count <- getCount
              count `shouldBe` 1
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "1 iteration"
        , testCase "warmupIterations = 3 makes 3 requests and logs plural 'iterations'" $
            mockCountedRequests status200 "{}" $ \port getCount -> do
              (ctx, logRef) <- makeCtxWithWarmupAndLog port 3
              let ep = makeEndpoint port
              runWarmup ctx ep
              count <- getCount
              count `shouldBe` 3
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "3 iterations"
        ]
    ]

-- Helpers

makeEndpoint :: Int -> Endpoint
makeEndpoint port =
  Endpoint
    { method = "GET"
    , url = T.pack $ "http://127.0.0.1:" ++ show port ++ "/"
    , body = Nothing
    , headers = []
    , validate = Nothing
    }

makeCtxWithWarmupAndLog :: Int -> Int -> IO (RunContext, IORef [(LogLevel, Text)])
makeCtxWithWarmupAndLog _port iters = do
  mgr <- newManager tlsManagerSettings
  logRef <- newIORef ([] :: [(LogLevel, Text)])
  let setts =
        (benchSettings makeValidConfig)
          { warmup = Just WarmupSettings {warmupIterations = iters}
          , secrets = Nothing
          }
      logger = makeCapturingLogger Debug logRef
  pure
    ( RunContext
        { rcSettings = setts
        , rcManager = mgr
        , rcToken = ""
        , rcCsvFile = "/dev/null"
        , rcTimestamp = "test"
        , rcEventChan = Nothing
        , rcLogger = logger
        , rcTargetName = ""
        }
    , logRef
    )
