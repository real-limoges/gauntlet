-- | Facade re-exporting HTTP networking primitives for benchmark execution.
module Benchmark.Network
  ( BenchmarkEnv (..)
  , initNetwork
  , runBenchmark
  , runBenchmarkDuration
  , timedRequest
  , addAuth
  , readToken
  , prepareRequest
  )
where

import Benchmark.Network.Auth (addAuth, readToken)
import Benchmark.Network.Exec
  ( BenchmarkEnv (..)
  , runBenchmark
  , runBenchmarkDuration
  )
import Benchmark.Network.Request (initNetwork, prepareRequest, timedRequest)
