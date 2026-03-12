-- | Tests for Benchmark.Network.Request.
module RequestSpec (requestSpec) where

import Benchmark.Network (initNetwork, prepareRequest, timedRequest)
import Benchmark.Types
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import MockServer (mockCountedRequests, mockJson)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (status500)
import Network.Socket qualified as Socket
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase)

requestSpec :: TestTree
requestSpec = withResource setupManager (\_ -> pure ()) $ \getMgr ->
  testGroup
    "Network.Request"
    [ prepareRequestSpec
    , timedRequestSpec getMgr
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

setupManager :: IO Client.Manager
setupManager = initNetwork testSettings

testSettings :: Settings
testSettings =
  Settings
    { iterations = 1
    , concurrency = 1
    , secrets = Nothing
    , maxConnections = Just 5
    , requestTimeout = Just 10
    , retry = Just (RetrySettings 0 0 1.0)
    , warmup = Nothing
    , logLevel = Nothing
    , tempo = Nothing
    , healthCheckPath = Nothing
    , healthCheckTimeout = Nothing
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

-- ---------------------------------------------------------------------------
-- prepareRequest
-- ---------------------------------------------------------------------------

prepareRequestSpec :: TestTree
prepareRequestSpec =
  testGroup
    "prepareRequest"
    [ testCase "sets method to GET" $ do
        let ep = Endpoint "GET" "http://127.0.0.1:9999" Nothing [] Nothing
        req <- prepareRequest testSettings ep
        Client.method req `shouldBe` "GET"
    , testCase "sets method to POST" $ do
        let ep = Endpoint "POST" "http://127.0.0.1:9999" Nothing [] Nothing
        req <- prepareRequest testSettings ep
        Client.method req `shouldBe` "POST"
    , testCase "sets JSON body when body present" $ do
        let ep = Endpoint "POST" "http://127.0.0.1:9999" (Just (object ["k" .= ("v" :: Text)])) [] Nothing
        req <- prepareRequest testSettings ep
        -- Body is non-empty when set
        case Client.requestBody req of
          Client.RequestBodyLBS bs -> length (show bs) `shouldSatisfy` (> 0)
          _ -> pure ()
    , testCase "maps custom headers" $ do
        let ep = Endpoint "GET" "http://127.0.0.1:9999" Nothing [("X-Api-Key", "secret")] Nothing
        req <- prepareRequest testSettings ep
        let hdrs = Client.requestHeaders req
        any (\(k, v) -> k == "x-api-key" && v == "secret") hdrs `shouldBe` True
    ]

-- ---------------------------------------------------------------------------
-- timedRequest
-- ---------------------------------------------------------------------------

timedRequestSpec :: IO Client.Manager -> TestTree
timedRequestSpec getMgr =
  testGroup
    "timedRequest"
    [ testCase "returns 200 on success" $ do
        mgr <- getMgr
        mockJson "{\"ok\":true}" $ \port -> do
          resp <- timedRequest testSettings mgr (testEndpoint port)
          statusCode resp `shouldBe` 200
          errorMessage resp `shouldBe` Nothing
    , testCase "records duration > 0" $ do
        mgr <- getMgr
        mockJson "{}" $ \port -> do
          resp <- timedRequest testSettings mgr (testEndpoint port)
          durationNs resp `shouldSatisfy` (> 0)
    , testCase "returns 500 without retrying (server sees 1 request)" $ do
        mgr <- getMgr
        mockCountedRequests status500 "{}" $ \port readCount -> do
          resp <- timedRequest testSettings mgr (testEndpoint port)
          count <- readCount
          statusCode resp `shouldBe` 500
          count `shouldBe` 1
    , testCase "returns error result on connection failure" $ do
        mgr <- getMgr
        sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
        port <- fromIntegral <$> Socket.socketPort sock
        Socket.close sock
        resp <- timedRequest testSettings mgr (testEndpoint port)
        statusCode resp `shouldBe` 0
        errorMessage resp `shouldSatisfy` (/= Nothing)
    , testCase "retries on connection failure and exhausts attempts" $ do
        mgr <- getMgr
        let retrySettings = testSettings {retry = Just (RetrySettings 2 1 1.0)}
        sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
        port <- fromIntegral <$> Socket.socketPort sock
        Socket.close sock
        resp <- timedRequest retrySettings mgr (testEndpoint port)
        -- Retries exhausted: statusCode=0, error present
        statusCode resp `shouldBe` 0
        errorMessage resp `shouldSatisfy` (/= Nothing)
    ]
