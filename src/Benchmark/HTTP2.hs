-- |
-- Module      : Benchmark.HTTP2
-- Description : HTTP/2 client for benchmark requests
-- Stability   : experimental
--
-- Provides a persistent HTTP/2 connection using the @http2@ library.
-- The connection is established once per benchmark run and all concurrent
-- benchmark threads share it via HTTP/2 native stream multiplexing.
--
-- Supports both:
--
-- * @https://@ — HTTP\/2 over TLS with ALPN @h2@ negotiation
-- * @http://@ — HTTP\/2 cleartext (h2c), suitable for internal services
--
-- == Architecture
--
-- 'initH2Connection' forks a background thread that calls
-- @Network.HTTP2.Client.run@.  The 'SendRequest' function is captured and
-- stored in the 'Http2Connection', making it callable from concurrent
-- benchmark threads without additional coordination.  The @http2@ library
-- manages stream allocation internally.
module Benchmark.HTTP2
  ( Http2Connection (..),
    initH2Connection,
    closeH2Connection,
    timedRequestH2,
    parseHostPort,
  )
where

import Benchmark.Types
  ( Endpoint (..),
    Nanoseconds (..),
    Settings,
    TestingResponse (..),
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (mk)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
import Data.Text.Encoding (encodeUtf8)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.HTTP.Types (RequestHeaders, Status)
import Network.HTTP.Types qualified as HTTPTypes
import Network.HTTP2.Client
  ( Client,
    SendRequest,
    allocSimpleConfig,
    defaultClientConfig,
    defaultConfig,
    freeSimpleConfig,
    getResponseBodyChunk,
    requestBuilder,
    requestNoBody,
    responseStatus,
    run,
    confBufferSize,
    confMySockAddr,
    confPeerSockAddr,
    confReadN,
    confSendAll,
    confWriteBuffer,
  )
import Network.HTTP2.Client qualified as H2
import Network.Socket (HostName, ServiceName, SockAddr)
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)

-- | Newtype wrapper that allows storing the polymorphic 'SendRequest' inside
-- containers like 'MVar' without requiring @ImpredicativeTypes@.
newtype Requester = Requester {runRequester :: forall r. H2.Request -> (H2.Response -> IO r) -> IO r}

-- | A live HTTP\/2 connection that can serve concurrent requests.
data Http2Connection = Http2Connection
  { -- | Thread-safe: calls @sendRequest@ inside the @http2@ run scope
    h2Requester :: Requester,
    -- | Signal the background thread to close the connection
    h2Close :: IO (),
    -- | Used as @:authority@ pseudo-header
    h2Authority :: ByteString
  }

-- ---------------------------------------------------------------------------
-- Connection lifecycle

-- | Establish an HTTP\/2 connection to @targetUrl@.
--
-- For @https://@ URLs a TLS session is established with ALPN @h2@.
-- For @http://@ URLs plain TCP (h2c) is used.
initH2Connection :: Settings -> Text -> IO Http2Connection
initH2Connection _settings targetUrl = do
  let (host, port) = parseHostPort targetUrl
      isTLS = T.isPrefixOf "https://" targetUrl
      authorityBS = encodeUtf8 host

  srMVar <- newEmptyMVar :: IO (MVar Requester)
  closeMVar <- newEmptyMVar :: IO (MVar ())

  _ <- forkIO $ do
    withSocket (T.unpack host) (show port) $ \sock ->
      if isTLS
        then runTLSConnection host sock srMVar closeMVar
        else runPlainConnection (T.unpack host) sock srMVar closeMVar

  requester <- takeMVar srMVar
  return
    Http2Connection
      { h2Requester = requester,
        h2Close = putMVar closeMVar (),
        h2Authority = authorityBS
      }

-- | Close the connection.
closeH2Connection :: Http2Connection -> IO ()
closeH2Connection = h2Close

-- ---------------------------------------------------------------------------
-- Request execution

-- | Execute a single timed HTTP\/2 request.
timedRequestH2 :: Http2Connection -> Endpoint -> IO TestingResponse
timedRequestH2 Http2Connection {h2Requester, h2Authority} ep = do
  let path = extractPath (url ep)
      methodBS = encodeUtf8 (method ep)
      extraHdrs = map (\(k, v) -> (mk (encodeUtf8 k), encodeUtf8 v)) (headers ep)
      req =
        case body ep of
          Nothing -> requestNoBody methodBS path extraHdrs
          Just b ->
            requestBuilder
              methodBS
              path
              extraHdrs
              (Builder.byteString (LBS.toStrict (encode b)))

  startTime <- getTime Monotonic

  result <-
    try
      ( runRequester h2Requester req $ \rsp -> do
          let code = maybe 0 HTTPTypes.statusCode (responseStatus rsp)
          bodyBytes <- collectBody rsp
          return (code, bodyBytes)
      ) ::
      IO (Either SomeException (Int, LBS.ByteString))

  endTime <- getTime Monotonic

  let duration = Nanoseconds $ fromIntegral (max 0 (toNanoSecs endTime - toNanoSecs startTime))

  return $ case result of
    Left err ->
      TestingResponse
        { durationNs = duration,
          statusCode = 0,
          respBody = Nothing,
          errorMessage = Just (show err)
        }
    Right (code, respBody) ->
      TestingResponse
        { durationNs = duration,
          statusCode = code,
          respBody = Just respBody,
          errorMessage = Nothing
        }

-- ---------------------------------------------------------------------------
-- Internal: connection setup

-- | Run plain-TCP HTTP\/2 (h2c) using allocSimpleConfig.
runPlainConnection :: HostName -> Socket.Socket -> MVar Requester -> MVar () -> IO ()
runPlainConnection host sock srMVar closeMVar =
  bracket
    (allocSimpleConfig sock 4096)
    freeSimpleConfig
    $ \conf -> do
        let cliConf = defaultClientConfig {H2.scheme = "http", H2.authority = host}
        run cliConf conf $ \sendRequest _aux -> do
          putMVar srMVar (Requester sendRequest)
          takeMVar closeMVar

-- | Run HTTP\/2 over TLS (h2).
runTLSConnection :: Text -> Socket.Socket -> MVar Requester -> MVar () -> IO ()
runTLSConnection host sock srMVar closeMVar = do
  let hostStr = T.unpack host
      baseParams = TLS.defaultParamsClient hostStr ""
      tlsParams =
        baseParams
          { TLS.clientHooks =
              (TLS.clientHooks baseParams)
                { TLS.onSuggestALPN = return (Just ["h2"])
                },
            TLS.clientSupported =
              (TLS.clientSupported baseParams)
                { TLS.supportedCiphers = TLS.ciphersuite_default,
                  TLS.supportedVersions = [TLS.TLS13, TLS.TLS12]
                }
          }

  ctx <- TLS.contextNew sock tlsParams
  TLS.handshake ctx

  mySockAddr <- Socket.getSocketName sock
  peerSockAddr <- Socket.getPeerName sock

  bracket (mallocBytes 4096) free $ \buf -> do
    bufRef <- newIORef BS.empty
    let conf =
          defaultConfig
            { confWriteBuffer = buf,
              confBufferSize = 4096,
              confSendAll = \bs -> TLS.sendData ctx (LBS.fromStrict bs),
              confReadN = tlsReadN ctx bufRef,
              confMySockAddr = mySockAddr,
              confPeerSockAddr = peerSockAddr
            }

    let cliConf = defaultClientConfig {H2.scheme = "https", H2.authority = hostStr}

    result <-
      try
        ( run cliConf conf $ \sendRequest _aux -> do
            putMVar srMVar (Requester sendRequest)
            takeMVar closeMVar
        ) ::
        IO (Either SomeException ())

    TLS.bye ctx

    case result of
      Left err -> ioError (userError ("HTTP/2 TLS connection failed: " ++ show err))
      Right () -> return ()

-- | Read exactly @n@ bytes from a TLS context, buffering partial records.
tlsReadN :: TLS.Context -> IORef ByteString -> Int -> IO ByteString
tlsReadN ctx bufRef n = go
  where
    go = do
      buf <- readIORef bufRef
      if BS.length buf >= n
        then do
          writeIORef bufRef (BS.drop n buf)
          return (BS.take n buf)
        else do
          chunk <- TLS.recvData ctx
          writeIORef bufRef (buf <> chunk)
          go

-- ---------------------------------------------------------------------------
-- Utilities

-- | Open a TCP connection and run an action with the socket.
withSocket :: HostName -> ServiceName -> (Socket.Socket -> IO a) -> IO a
withSocket host port action = do
  addrs <- Socket.getAddrInfo Nothing (Just host) (Just port)
  case addrs of
    [] -> ioError $ userError $ "withSocket: cannot resolve " ++ host
    (addr : _) ->
      bracket
        (Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol)
        Socket.close
        $ \sock -> do
            Socket.connect sock (Socket.addrAddress addr)
            action sock

-- | Collect all response body chunks.
collectBody :: H2.Response -> IO LBS.ByteString
collectBody rsp = go []
  where
    go acc = do
      chunk <- getResponseBodyChunk rsp
      if BS.null chunk
        then return $ LBS.fromChunks (reverse acc)
        else go (chunk : acc)

-- ---------------------------------------------------------------------------
-- URL parsing

parseHostPort :: Text -> (Text, Int)
parseHostPort rawUrl =
  let noScheme = stripScheme rawUrl
      (hostPort, _) = T.breakOn "/" noScheme
      (host, portPart) = T.breakOn ":" hostPort
   in if T.null portPart
        then (host, defaultPort rawUrl)
        else (host, maybe (defaultPort rawUrl) id (readMaybe (T.unpack (T.drop 1 portPart))))

defaultPort :: Text -> Int
defaultPort url
  | T.isPrefixOf "https://" url = 443
  | otherwise = 80

extractPath :: Text -> ByteString
extractPath rawUrl =
  let noScheme = stripScheme rawUrl
      (_, pathPart) = T.breakOn "/" noScheme
   in encodeUtf8 $ if T.null pathPart then "/" else pathPart

stripScheme :: Text -> Text
stripScheme t
  | T.isPrefixOf "https://" t = T.drop 8 t
  | T.isPrefixOf "http://" t = T.drop 7 t
  | otherwise = t
