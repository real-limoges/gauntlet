module MockServer (
    withMockServer,
    mockJson,
    mockStatus,
    mockFailThenSucceed,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (Status, status200, status500)
import Network.Socket qualified as Socket
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp qualified as Warp

-- | Run action with mock server returning given status and body
withMockServer :: Status -> ByteString -> (Int -> IO a) -> IO a
withMockServer status body action = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen sock 1024
    port <- fromIntegral <$> Socket.socketPort sock
    ready <- newEmptyMVar
    let settings = Warp.setBeforeMainLoop (putMVar ready ()) Warp.defaultSettings
        app _ respond = respond $ responseLBS status [("Content-Type", "application/json")] body
    tid <- forkIO $ Warp.runSettingsSocket settings sock app
    takeMVar ready
    action port `finally` killThread tid

-- | Mock server returning 200 with JSON body
mockJson :: ByteString -> (Int -> IO a) -> IO a
mockJson = withMockServer status200

-- | Mock server returning given status with empty JSON
mockStatus :: Status -> (Int -> IO a) -> IO a
mockStatus status = withMockServer status "{}"

-- | Mock server that fails with 500 for the first N requests, then succeeds
mockFailThenSucceed :: Int -> (Int -> IO a) -> IO a
mockFailThenSucceed failCount action = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen sock 1024
    port <- fromIntegral <$> Socket.socketPort sock
    ready <- newEmptyMVar
    counter <- newMVar 0
    let settings = Warp.setBeforeMainLoop (putMVar ready ()) Warp.defaultSettings
        app _ respond = do
            count <- modifyMVar counter (\n -> return (n + 1, n))
            if count < failCount
                then respond $ responseLBS status500 [("Content-Type", "application/json")] "{\"error\":\"temporary failure\"}"
                else respond $ responseLBS status200 [("Content-Type", "application/json")] "{\"ok\":true}"
    tid <- forkIO $ Warp.runSettingsSocket settings sock app
    takeMVar ready
    action port `finally` killThread tid
