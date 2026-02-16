-- |
-- Module      : Benchmark.Network
-- Description : HTTP client for benchmark requests
-- Stability   : experimental
--
-- Provides connection pooling, request preparation, and timed HTTP execution
-- with automatic retry logic for failed requests.
module Benchmark.Network
  ( initNetwork,
    runBenchmark,
    runBenchmarkWithEvents,
    runComparison,
    timedRequest,
    addAuth,
    readToken,
    prepareRequest,
  )
where

import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types (Endpoint (..), LogLevel (..), Nanoseconds (..), PerfTestError (..), Settings (..), TestingResponse (..), defaultLogLevel, defaultRetrySettings)
import Benchmark.Types qualified as Types
import Control.Concurrent (QSem, threadDelay)
import Control.Concurrent.Async (concurrently, replicateConcurrently)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (SomeException, bracket_, evaluate, try)
import Control.Monad (when)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (mk)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Log (Logger, logInfo, logWarning, makeLogger)
import Network.HTTP.Client (Manager, ManagerSettings (..), Request, RequestBody (RequestBodyLBS), Response, httpLbs, newManager, parseRequest, responseStatus, responseTimeoutMicro)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status qualified as Status
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)

-- | Create an HTTP manager with connection pooling configured from settings.
initNetwork :: Settings -> IO Manager
initNetwork settings =
  newManager $
    tlsManagerSettings
      { managerConnCount = fromMaybe 10 (maxConnections settings),
        managerIdleConnectionCount = fromMaybe 10 (maxConnections settings),
        managerResponseTimeout = responseTimeoutMicro (fromMaybe 30 (Types.requestTimeout settings) * 1_000_000)
      }

readToken :: FilePath -> IO (Either PerfTestError Text)
readToken path = do
  result <- try (TIO.readFile path) :: IO (Either SomeException Text)
  return $ case result of
    Left err -> Left (TokenReadError path (show err))
    Right raw -> Right (T.strip raw)

addAuth :: Text -> Endpoint -> Endpoint
addAuth token ep =
  let authHeader = ("Authorization", "Bearer " <> token)
   in ep {headers = authHeader : headers ep}

getTimestamp :: IO String
getTimestamp = formatTime defaultTimeLocale "[%H:%M:%S]" <$> getZonedTime

-- | Pre-parse URL and configure request for reuse across iterations.
-- This avoids URL parsing overhead on each request.
prepareRequest :: Settings -> Endpoint -> IO Request
prepareRequest settings Endpoint {..} = do
  initialReq <- parseRequest (T.unpack url)
  let jsonBody = maybe mempty (RequestBodyLBS . encode) body
      timeoutSecs = fromMaybe 30 (Types.requestTimeout settings)

  return $
    initialReq
      { Client.method = encodeUtf8 method,
        Client.requestBody = jsonBody,
        Client.requestHeaders = map (\(k, v) -> (mk $ encodeUtf8 k, encodeUtf8 v)) headers,
        Client.responseTimeout = responseTimeoutMicro (timeoutSecs * 1_000_000),
        Client.shouldStripHeaderOnRedirect = const False
      }

timedRequestPrepared :: Settings -> Manager -> Request -> IO TestingResponse
timedRequestPrepared settings mgr req = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
      retrySettings = maybe defaultRetrySettings id (Types.retry settings)
      maxAttempts = Types.retryMaxAttempts retrySettings
      initialDelay = Types.retryInitialDelayMs retrySettings * 1000 -- Convert to microseconds
      backoffMult = Types.retryBackoffMultiplier retrySettings
  retry logger maxAttempts initialDelay backoffMult
  where
    retry :: Logger -> Int -> Int -> Double -> IO TestingResponse
    retry logger attempts delay backoff = do
      startTime <- getTime Monotonic
      result <- try (httpLbs req mgr) :: IO (Either SomeException (Response LBS.ByteString))

      case result of
        Right resp -> processResponse resp startTime
        Left err | attempts > 0 -> do
          logWarning logger $ T.pack $ "Connection Failed: " ++ show err ++ " - Retrying (" ++ show attempts ++ " left)..."
          threadDelay delay
          retry logger (attempts - 1) (round (fromIntegral delay * backoff)) backoff
        Left err -> do
          endTime <- getTime Monotonic
          logWarning logger $ T.pack $ "FINAL FAILURE: " ++ show err
          return $
            TestingResponse
              { durationNs = Nanoseconds $ fromIntegral (toNanoSecs endTime - toNanoSecs startTime),
                statusCode = 0,
                respBody = Nothing,
                errorMessage = Just (show err)
              }

    processResponse resp startTime = do
      let body = Client.responseBody resp
      let code = Status.statusCode (responseStatus resp)
      _ <- evaluate (LBS.length body)
      endTime <- getTime Monotonic
      return $
        TestingResponse
          { durationNs = Nanoseconds $ fromIntegral (toNanoSecs endTime - toNanoSecs startTime),
            statusCode = code,
            respBody = Just body,
            errorMessage = Nothing
          }

-- | Prepare and execute a single timed request.
-- For multiple iterations, use 'runBenchmark' which prepares once.
timedRequest :: Settings -> Manager -> Endpoint -> IO TestingResponse
timedRequest settings mgr endpoint = do
  req <- prepareRequest settings endpoint
  timedRequestPrepared settings mgr req

-- | Run concurrent benchmark iterations with rate limiting via semaphore.
-- Prepares the request once and reuses it for all iterations.
runBenchmark :: Settings -> QSem -> Manager -> Int -> Int -> Endpoint -> IO [TestingResponse]
runBenchmark settings sem mgr iters pIdx endpoint = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
  preparedReq <- prepareRequest settings endpoint
  countRef <- newIORef 0

  replicateConcurrently iters $ do
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      res <- timedRequestPrepared settings mgr preparedReq

      current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
      printProgressBar logger pIdx current iters

      return res
  where
    printProgressBar logger idx c total = do
      let numUpdates = min total 10
          step = max 1 (total `div` numUpdates)
          shouldPrint = c > 0 && (c `mod` step == 0 || c == total)

      when shouldPrint $ do
        let percent = (c * 100) `div` total
        logInfo logger $ T.pack $ "[Ep " ++ show idx ++ "] Progress: " ++ show percent ++ "% (" ++ show c ++ "/" ++ show total ++ ")"

-- | Run benchmark with event emission for TUI updates.
-- Emits RequestCompleted/RequestFailed after each request.
runBenchmarkWithEvents :: Settings -> QSem -> Manager -> Int -> Int -> Endpoint -> TChan BenchmarkEvent -> IO [TestingResponse]
runBenchmarkWithEvents settings sem mgr iters pIdx endpoint eventChan = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
  preparedReq <- prepareRequest settings endpoint
  countRef <- newIORef 0

  replicateConcurrently iters $ do
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      res <- timedRequestPrepared settings mgr preparedReq

      -- Emit event for TUI
      let event = case errorMessage res of
            Nothing -> RequestCompleted (durationNs res) (statusCode res)
            Just err -> RequestFailed (T.pack err)
      atomically $ writeTChan eventChan event

      current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
      printProgressBar logger pIdx current iters

      return res
  where
    printProgressBar logger idx c total = do
      let numUpdates = min total 10
          step = max 1 (total `div` numUpdates)
          shouldPrint = c > 0 && (c `mod` step == 0 || c == total)

      when shouldPrint $ do
        let percent = (c * 100) `div` total
        logInfo logger $ T.pack $ "[Ep " ++ show idx ++ "] Progress: " ++ show percent ++ "% (" ++ show c ++ "/" ++ show total ++ ")"

-- | Execute two endpoints concurrently for A/B comparison.
runComparison :: Settings -> Manager -> Endpoint -> Endpoint -> IO (TestingResponse, TestingResponse)
runComparison settings mgr endpointA endpointB = do
  reqA <- prepareRequest settings endpointA
  reqB <- prepareRequest settings endpointB
  concurrently (timedRequestPrepared settings mgr reqA) (timedRequestPrepared settings mgr reqB)
