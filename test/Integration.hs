-- | End-to-end integration tests for HTTP operations.
module Integration (integrationSpec) where

import Benchmark.Network hiding (prepareRequest)
import Benchmark.Report.Baseline
import Benchmark.Types
import Control.Concurrent.QSem (newQSem)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import MockServer
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (status500)
import Network.Socket qualified as Socket
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import TastyCompat (shouldBe, shouldContain, shouldSatisfy)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)

integrationSpec :: TestTree
integrationSpec = withResource setupManager (\_ -> pure ()) $ \getMgr ->
  testGroup
    "Integration Tests"
    [ testGroup
        "timedRequest"
        [ testCase "makes successful HTTP request" $ do
            mgr <- getMgr
            mockJson "{\"ok\":true}" $ \port -> do
              resp <- timedRequest testSettings mgr (endpoint port)
              statusCode resp `shouldBe` 200
              errorMessage resp `shouldBe` Nothing
              durationNs resp `shouldSatisfy` (> 0)
        , testCase "handles POST with body" $ do
            mgr <- getMgr
            mockJson "{}" $ \port -> do
              let ep = (endpoint port) {method = "POST", body = Just (object ["k" .= ("v" :: Text)])}
              resp <- timedRequest testSettings mgr ep
              statusCode resp `shouldBe` 200
        ]
    , testGroup
        "runBenchmark"
        [ testCase "runs multiple iterations successfully" $ do
            mgr <- getMgr
            mockJson "{}" $ \port -> do
              sem <- newQSem 4
              let env = BenchmarkEnv testSettings sem mgr 1 Nothing
              results <- runBenchmark env 5 (endpoint port) Nothing
              length results `shouldBe` 5
              all ((== 200) . statusCode) results `shouldBe` True
        ]
    , testGroup
        "error handling"
        [ testCase "handles HTTP 500" $ do
            mgr <- getMgr
            mockStatus status500 $ \port -> do
              resp <- timedRequest testSettings mgr (endpoint port)
              statusCode resp `shouldBe` 500
        ]
    , testGroup
        "retry logic"
        [ testCase "does not retry on HTTP 500 (server receives exactly 1 request)" $ do
            mgr <- getMgr
            mockCountedRequests status500 "{}" $ \port readCount -> do
              resp <- timedRequest testSettings mgr (endpoint port)
              count <- readCount
              statusCode resp `shouldBe` 500
              errorMessage resp `shouldBe` Nothing
              count `shouldBe` 1
        , testCase "returns error result on connection failure" $ do
            mgr <- getMgr
            -- Bind to a free port then close without listening so the port is unreachable
            sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
            Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
            port <- fromIntegral <$> Socket.socketPort sock
            Socket.close sock
            let noRetry = testSettings {retry = Just (RetrySettings 0 0 1.0)}
            resp <- timedRequest noRetry mgr (endpoint port)
            errorMessage resp `shouldSatisfy` (/= Nothing)
        ]
    , testGroup
        "custom headers in HTTP requests"
        [ testCase "sends custom headers to server" $ do
            mgr <- getMgr
            mockJson "{}" $ \port -> do
              let customHeaders = [("X-API-Key", "secret123"), ("X-Request-ID", "test-456")]
              let ep = (endpoint port) {headers = customHeaders}
              resp <- timedRequest testSettings mgr ep
              statusCode resp `shouldBe` 200
        , testCase "overrides Content-Type with custom header" $ do
            mgr <- getMgr
            mockJson "{}" $ \port -> do
              let customHeaders = [("Content-Type", "text/xml")]
              let ep = (endpoint port) {headers = customHeaders, method = "POST"}
              resp <- timedRequest testSettings mgr ep
              statusCode resp `shouldBe` 200
        ]
    , testGroup
        "baseline operations"
        [ testCase "saves and loads baseline correctly" $ do
            let stats = testBenchmarkStats
            -- Save
            saveResult <- saveBaseline "test-baseline" "2024-01-01T00:00:00" stats
            case saveResult of
              Left err -> assertFailure $ "Save failed: " ++ err
              Right path -> do
                -- Load
                loadResult <- loadBaseline "test-baseline"
                case loadResult of
                  Left err -> assertFailure $ "Load failed: " ++ err
                  Right baseline -> do
                    baselineStats baseline `shouldBe` stats
                    baselineName baseline `shouldBe` "test-baseline"
                -- Cleanup
                removeFile path `catchIOError` const (return ())
        , testCase "returns error for missing baseline" $ do
            result <- loadBaseline "nonexistent-baseline-xyz"
            case result of
              Left _ -> return () -- Expected
              Right _ -> assertFailure "Expected error for missing baseline"
        , testCase "lists saved baselines" $ do
            -- Create test baselines
            _ <- saveBaseline "list-test-1" "2024-01-01" testBenchmarkStats
            _ <- saveBaseline "list-test-2" "2024-01-02" testBenchmarkStats
            baselines <- listBaselines
            baselines `shouldContain` ["list-test-1"]
            baselines `shouldContain` ["list-test-2"]
            -- Cleanup
            removeFile (baselineDir ++ "/list-test-1.json") `catchIOError` const (return ())
            removeFile (baselineDir ++ "/list-test-2.json") `catchIOError` const (return ())
        ]
    ]

setupManager :: IO Manager
setupManager = newManager defaultManagerSettings

testSettings :: Settings
testSettings =
  Settings
    10
    4
    Nothing
    (Just 10)
    (Just 30)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

endpoint :: Int -> Endpoint
endpoint port = Endpoint "GET" ("http://127.0.0.1:" <> T.pack (show port)) Nothing [] Nothing

testBenchmarkStats :: BenchmarkStats
testBenchmarkStats =
  BenchmarkStats
    { totalRequests = 100
    , countSuccess = 98
    , countFailure = 2
    , meanMs = 45.5
    , stdDevMs = 12.3
    , minMs = 20.0
    , maxMs = 150.0
    , p50Ms = 42.0
    , p95Ms = 75.0
    , p99Ms = 120.0
    , esMs = 135.0
    }
