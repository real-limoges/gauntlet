{- |
Module      : Benchmark.Network.Pool
Description : HTTP transport handle and connection pool initialisation
-}
module Benchmark.Network.Pool (
    NetworkHandle (..),
    initNetwork,
    initNetworkHandle,
)
where

import Benchmark.HTTP2 (Http2Connection, initH2Connection)
import Benchmark.Types (Settings (..))
import Benchmark.Types qualified as Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Discriminated union over the two supported HTTP transports.
data NetworkHandle
    = -- | HTTP\/1.1 with a connection-pooling 'Manager'
      H1 Manager
    | -- | HTTP\/2 with a persistent multiplexed connection
      H2 Http2Connection

-- | Create an HTTP manager with connection pooling configured from settings.
initNetwork :: Settings -> IO Manager
initNetwork settings =
    newManager $
        tlsManagerSettings
            { managerConnCount = fromMaybe 10 (maxConnections settings)
            , managerIdleConnectionCount = fromMaybe 10 (maxConnections settings)
            , managerResponseTimeout = responseTimeoutMicro (fromMaybe 30 (Types.requestTimeout settings) * 1_000_000)
            }

{- | Create the appropriate 'NetworkHandle' based on the @httpVersion@ setting.
@"2"@ → persistent HTTP\/2 connection to @targetUrl@.
Any other value (or omitted) → HTTP\/1.1 'Manager'.
-}
initNetworkHandle :: Settings -> Text -> IO NetworkHandle
initNetworkHandle settings targetUrl =
    case fromMaybe "1.1" (Types.httpVersion settings) of
        "2" -> H2 <$> initH2Connection settings targetUrl
        _ -> H1 <$> initNetwork settings
