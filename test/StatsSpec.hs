module StatsSpec (statsSpec) where

import Benchmark.Types (BenchmarkStats (..))
import Stats.Benchmark (calculateStats)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers

statsSpec :: TestTree
statsSpec =
  testGroup
    "Stats.Benchmark"
    [ testGroup
        "calculateStats"
        [ testCase "handles single response" $ do
            let r = makeResult 50_000_000
            let stats = calculateStats [r]
            meanMs stats `shouldBe` 50.0
            stdDevMs stats `shouldBe` 0.0
            p50Ms stats `shouldBe` 50.0
            p95Ms stats `shouldBe` 50.0
            p99Ms stats `shouldBe` 50.0
        , testCase "handles empty response list" $ do
            let stats = calculateStats []
            totalRequests stats `shouldBe` 0
            countSuccess stats `shouldBe` 0
            meanMs stats `shouldBe` 0.0
        , testCase "excludes failed responses from duration calculations" $ do
            let r1 = makeResult 10_000_000
            let r2 = makeErrorResult "Connection timeout"
            let r3 = makeResult 30_000_000
            let stats = calculateStats [r1, r2, r3]
            totalRequests stats `shouldBe` 3
            countSuccess stats `shouldBe` 2
            countFailure stats `shouldBe` 1
            meanMs stats `shouldBe` 20.0
        , testCase "calculates correct percentiles for known data" $ do
            let responses = map (makeResult . (* 1_000_000)) [1 .. 10]
            let stats = calculateStats responses
            p50Ms stats `shouldSatisfy` (\x -> x > 5.0 && x < 6.0)
            p99Ms stats `shouldSatisfy` (> 9.0)
        ]
    ]
