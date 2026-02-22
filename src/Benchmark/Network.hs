-- |
-- Module      : Benchmark.Network
-- Description : HTTP client for benchmark requests
-- Stability   : experimental
--
-- Re-exports the full public API from the Network sub-modules:
--
-- * "Benchmark.Network.Pool"    — transport handle and connection pool
-- * "Benchmark.Network.Auth"    — token loading and Bearer auth injection
-- * "Benchmark.Network.Request" — request preparation and timed execution
-- * "Benchmark.Network.Exec"    — concurrent benchmark loops and A/B comparison
module Benchmark.Network
  ( NetworkHandle (..),
    initNetworkHandle,
    initNetwork,
    runBenchmark,
    runBenchmarkWithEvents,
    runComparison,
    timedRequest,
    addAuth,
    readToken,
    prepareRequest,
  )
where

import Benchmark.Network.Auth (addAuth, readToken)
import Benchmark.Network.Exec (runBenchmark, runBenchmarkWithEvents, runComparison)
import Benchmark.Network.Pool (NetworkHandle (..), initNetwork, initNetworkHandle)
import Benchmark.Network.Request (prepareRequest, timedRequest)
