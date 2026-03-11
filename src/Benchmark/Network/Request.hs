module Benchmark.Network.Request
  ( initNetwork
  , prepareRequest
  , timedRequest
  , timedRequestPrepared
  )
where

import Benchmark.Types
  ( Endpoint (..)
  , Nanoseconds (..)
  , Settings (..)
  , TestingResponse (..)
  , defaultLogLevel
  , defaultRetrySettings
  )
import Benchmark.Types qualified as Types
import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Log (Logger, logWarning, makeLogger)
import Network.HTTP.Client
  ( HttpException (..)
  , Manager
  , ManagerSettings (..)
  , Request
  , RequestBody (RequestBodyLBS)
  , Response
  , httpLbs
  , newManager
  , parseRequest
  , responseStatus
  , responseTimeoutMicro
  )
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status qualified as Status
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)

-- | Create an HTTP manager with connection pooling configured from settings.
initNetwork :: Settings -> IO Manager
initNetwork settings =
  newManager $
    tlsManagerSettings
      { managerConnCount = fromMaybe 10 (maxConnections settings)
      , managerIdleConnectionCount = fromMaybe 10 (maxConnections settings)
      , managerResponseTimeout = responseTimeoutMicro (fromMaybe 30 (Types.requestTimeout settings) * 1_000_000)
      }

{-| Prepare and execute a single timed request.
For multiple iterations, prefer 'runBenchmark' which amortises preparation cost.
-}
timedRequest :: Settings -> Manager -> Endpoint -> IO TestingResponse
timedRequest settings mgr ep = do
  req <- prepareRequest settings ep
  timedRequestPrepared settings mgr req

{-| Pre-parse URL and configure request for reuse across iterations.
This avoids URL parsing overhead on each request.
-}
prepareRequest :: Settings -> Endpoint -> IO Request
prepareRequest settings Endpoint {..} = do
  initialReq <- parseRequest (T.unpack url)
  let jsonBody = maybe mempty (RequestBodyLBS . encode) body
      timeoutSecs = fromMaybe 30 (Types.requestTimeout settings)
  return $
    initialReq
      { Client.method = encodeUtf8 method
      , Client.requestBody = jsonBody
      , Client.requestHeaders = map (\(k, v) -> (mk $ encodeUtf8 k, encodeUtf8 v)) headers
      , Client.responseTimeout = responseTimeoutMicro (timeoutSecs * 1_000_000)
      , Client.shouldStripHeaderOnRedirect = const False
      }

-- | Execute a pre-parsed request with retry logic.
timedRequestPrepared :: Settings -> Manager -> Request -> IO TestingResponse
timedRequestPrepared settings mgr req = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
      retrySettings = fromMaybe defaultRetrySettings (Types.retry settings)
      maxAttempts = Types.retryMaxAttempts retrySettings
      initialDelay = Types.retryInitialDelayMs retrySettings * 1000
      backoffMult = Types.retryBackoffMultiplier retrySettings
  go logger maxAttempts initialDelay backoffMult
  where
    go :: Logger -> Int -> Int -> Double -> IO TestingResponse
    go logger attempts delay backoff = do
      startTime <- getTime Monotonic
      result <- try (httpLbs req mgr) :: IO (Either SomeException (Response LBS.ByteString))
      case result of
        Right resp -> processResponse resp startTime
        Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
        Left err | attempts > 0 -> do
          logWarning logger $
            T.pack $
              "Connection failed: " ++ showHttpErr err ++ " - Retrying (" ++ show attempts ++ " left)..."
          threadDelay delay
          go logger (attempts - 1) (round (fromIntegral delay * backoff)) backoff
        Left err -> do
          endTime <- getTime Monotonic
          logWarning logger $ T.pack $ "Connection failed (no retries left): " ++ showHttpErr err
          return $
            TestingResponse
              { durationNs = Nanoseconds $ fromIntegral (max 0 (toNanoSecs endTime - toNanoSecs startTime))
              , statusCode = 0
              , respBody = Nothing
              , errorMessage = Just (show err)
              }

    showHttpErr e = case fromException e of
      Just (HttpExceptionRequest _ content) -> show content
      _ -> show e

    processResponse resp startTime = do
      let body = Client.responseBody resp
          code = Status.statusCode (responseStatus resp)
      _ <- evaluate (LBS.length body)
      endTime <- getTime Monotonic
      return $
        TestingResponse
          { durationNs = Nanoseconds $ fromIntegral (max 0 (toNanoSecs endTime - toNanoSecs startTime))
          , statusCode = code
          , respBody = Just body
          , errorMessage = Nothing
          }
