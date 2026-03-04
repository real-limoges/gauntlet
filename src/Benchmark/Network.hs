{-|
Module      : Benchmark.Network
Description : HTTP client for benchmark requests
Stability   : experimental

Re-exports the full public API from the Network sub-modules:

* "Benchmark.Network.Auth"    — token loading and Bearer auth injection
* "Benchmark.Network.Request" — connection pool, request preparation, and timed execution
* "Benchmark.Network.Exec"    — concurrent benchmark loops and A/B comparison
-}
module Benchmark.Network
  ( initNetwork
  , runBenchmark
  , runBenchmarkWithEvents
  , runBenchmarkDuration
  , runBenchmarkDurationWithEvents
  , timedRequest
  , addAuth
  , readToken
  , prepareRequest
  )
where

import Benchmark.Network.Auth (addAuth, readToken)
import Benchmark.Network.Exec
  ( runBenchmark
  , runBenchmarkDuration
  , runBenchmarkDurationWithEvents
  , runBenchmarkWithEvents
  )
import Benchmark.Network.Request (initNetwork, prepareRequest, timedRequest)
