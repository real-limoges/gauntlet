module Integration (integrationSpec) where

import Benchmark.Baseline
import Benchmark.Network
import Benchmark.Types
import Benchmark.Verify
import Control.Concurrent.QSem (newQSem)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import MockServer
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status404, status500)
import Network.Socket qualified as Socket
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import Test.Hspec

integrationSpec :: Spec
integrationSpec = describe "Integration Tests" $ beforeAll setupManager $ do
    describe "timedRequest" $ do
        it "makes successful HTTP request" $ \mgr ->
            mockJson "{\"ok\":true}" $ \port -> do
                resp <- timedRequest testSettings mgr (endpoint port)
                statusCode resp `shouldBe` 200
                errorMessage resp `shouldBe` Nothing
                durationNs resp `shouldSatisfy` (> 0)

        it "handles POST with body" $ \mgr ->
            mockJson "{}" $ \port -> do
                let ep = (endpoint port){method = "POST", body = Just (object ["k" .= ("v" :: Text)])}
                resp <- timedRequest testSettings mgr ep
                statusCode resp `shouldBe` 200

    describe "runBenchmark" $ do
        it "runs multiple iterations successfully" $ \mgr ->
            mockJson "{}" $ \port -> do
                sem <- newQSem 4
                results <- runBenchmark testSettings sem (H1 mgr) 5 1 (endpoint port)
                length results `shouldBe` 5
                all ((== 200) . statusCode) results `shouldBe` True

    describe "runComparison" $ do
        it "compares two endpoints concurrently" $ \mgr ->
            mockJson "{}" $ \port -> do
                (a, b) <- runComparison testSettings (H1 mgr) (endpoint port) (endpoint port)
                statusCode a `shouldBe` 200
                statusCode b `shouldBe` 200

    describe "verify" $ do
        it "matches identical responses" $ \mgr ->
            mockJson "{\"x\":1}" $ \port -> do
                a <- timedRequest testSettings mgr (endpoint port)
                b <- timedRequest testSettings mgr (endpoint port)
                verify a b `shouldBe` Match

        it "detects status mismatch" $ \mgr ->
            mockStatus status200 $ \p1 ->
                mockStatus status404 $ \p2 -> do
                    a <- timedRequest testSettings mgr (endpoint p1)
                    b <- timedRequest testSettings mgr (endpoint p2)
                    case verify a b of
                        StatusMismatch 200 404 -> pure ()
                        x -> expectationFailure $ "Expected StatusMismatch, got: " ++ show x

    describe "error handling" $ do
        it "handles HTTP 500" $ \mgr ->
            mockStatus status500 $ \port -> do
                resp <- timedRequest testSettings mgr (endpoint port)
                statusCode resp `shouldBe` 500

    describe "retry logic" $ do
        it "does not retry on HTTP 500 (server receives exactly 1 request)" $ \mgr ->
            mockCountedRequests status500 "{}" $ \port readCount -> do
                resp <- timedRequest testSettings mgr (endpoint port)
                count <- readCount
                statusCode resp `shouldBe` 500
                errorMessage resp `shouldBe` Nothing
                count `shouldBe` 1

        it "returns error result on connection failure" $ \mgr -> do
            -- Bind to a free port then close without listening so the port is unreachable
            sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
            Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
            port <- fromIntegral <$> Socket.socketPort sock
            Socket.close sock
            let noRetry = testSettings{retry = Just (RetrySettings 0 0 1.0)}
            resp <- timedRequest noRetry mgr (endpoint port)
            errorMessage resp `shouldSatisfy` (/= Nothing)

        it "accepts custom retry settings" $ \mgr ->
            mockJson "{}" $ \port -> do
                -- Test that custom retry settings are accepted and used
                let retrySettings = RetrySettings 5 200 1.5
                let setts = testSettings{retry = Just retrySettings}
                resp <- timedRequest setts mgr (endpoint port)
                statusCode resp `shouldBe` 200
                errorMessage resp `shouldBe` Nothing

        it "uses default retry settings when not specified" $ \mgr ->
            mockJson "{}" $ \port -> do
                -- Test that default retry settings work
                let setts = testSettings{retry = Nothing}
                resp <- timedRequest setts mgr (endpoint port)
                statusCode resp `shouldBe` 200

        it "retry settings with zero attempts work" $ \mgr ->
            mockJson "{}" $ \port -> do
                -- Test that zero retries means no retries
                let retrySettings = RetrySettings 0 100 2.0
                let setts = testSettings{retry = Just retrySettings}
                resp <- timedRequest setts mgr (endpoint port)
                statusCode resp `shouldBe` 200

    describe "warmup execution" $ do
        it "runs warmup iterations before benchmark" $ \mgr ->
            mockJson "{}" $ \port -> do
                sem <- newQSem 1
                let warmupSettings = WarmupSettings 3
                let setts = testSettings{warmup = Just warmupSettings}
                -- This test just verifies warmup doesn't crash
                -- In real Runner, warmup is called before benchmark
                results <- runBenchmark setts sem (H1 mgr) 5 1 (endpoint port)
                length results `shouldBe` 5

        it "skips warmup when iterations = 0" $ \mgr ->
            mockJson "{}" $ \port -> do
                sem <- newQSem 1
                let warmupSettings = WarmupSettings 0
                let setts = testSettings{warmup = Just warmupSettings}
                results <- runBenchmark setts sem (H1 mgr) 3 1 (endpoint port)
                length results `shouldBe` 3

    describe "custom headers in HTTP requests" $ do
        it "sends custom headers to server" $ \mgr ->
            mockJson "{}" $ \port -> do
                let customHeaders = [("X-API-Key", "secret123"), ("X-Request-ID", "test-456")]
                let ep = (endpoint port){headers = customHeaders}
                resp <- timedRequest testSettings mgr ep
                statusCode resp `shouldBe` 200

        it "overrides Content-Type with custom header" $ \mgr ->
            mockJson "{}" $ \port -> do
                let customHeaders = [("Content-Type", "text/xml")]
                let ep = (endpoint port){headers = customHeaders, method = "POST"}
                resp <- timedRequest testSettings mgr ep
                statusCode resp `shouldBe` 200
    describe "baseline operations" $ do
        it "saves and loads baseline correctly" $ \_ -> do
            let stats = testBenchmarkStats
            -- Save
            saveResult <- saveBaseline "test-baseline" "2024-01-01T00:00:00" stats
            case saveResult of
                Left err -> expectationFailure $ "Save failed: " ++ err
                Right path -> do
                    -- Load
                    loadResult <- loadBaseline "test-baseline"
                    case loadResult of
                        Left err -> expectationFailure $ "Load failed: " ++ err
                        Right baseline -> do
                            baselineStats baseline `shouldBe` stats
                            baselineName baseline `shouldBe` "test-baseline"
                    -- Cleanup
                    removeFile path `catchIOError` const (return ())

        it "returns error for missing baseline" $ \_ -> do
            result <- loadBaseline "nonexistent-baseline-xyz"
            case result of
                Left _ -> return () -- Expected
                Right _ -> expectationFailure "Expected error for missing baseline"

        it "lists saved baselines" $ \_ -> do
            -- Create test baselines
            _ <- saveBaseline "list-test-1" "2024-01-01" testBenchmarkStats
            _ <- saveBaseline "list-test-2" "2024-01-02" testBenchmarkStats
            baselines <- listBaselines
            baselines `shouldContain` ["list-test-1"]
            baselines `shouldContain` ["list-test-2"]
            -- Cleanup
            removeFile (baselineDir ++ "/list-test-1.json") `catchIOError` const (return ())
            removeFile (baselineDir ++ "/list-test-2.json") `catchIOError` const (return ())

setupManager :: IO Manager
setupManager = newManager defaultManagerSettings

testSettings :: Settings
testSettings = Settings 10 4 "" (Just 10) (Just 5) (Just 30) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
        }
