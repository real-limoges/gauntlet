{- |
Module      : Benchmark.Network.Pool
Description : HTTP connection pool initialisation
-}
module Benchmark.Network.Pool (
    initNetwork,
)
where

import Benchmark.Types (Settings (..))
import Benchmark.Types qualified as Types
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Create an HTTP manager with connection pooling configured from settings.
initNetwork :: Settings -> IO Manager
initNetwork settings =
    newManager $
        tlsManagerSettings
            { managerConnCount = fromMaybe 10 (maxConnections settings)
            , managerIdleConnectionCount = fromMaybe 10 (maxConnections settings)
            , managerResponseTimeout = responseTimeoutMicro (fromMaybe 30 (Types.requestTimeout settings) * 1_000_000)
            }
