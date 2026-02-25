module StatsSpec (statsSpec) where

import Benchmark.Types (BenchmarkStats (..))
import Data.Vector.Unboxed qualified as V
import Stats.Benchmark (calculateStats)
import Stats.Common (variance, varianceList)
import Test.Hspec
import TestHelpers

statsSpec :: Spec
statsSpec = describe "Stats.Benchmark" $ do
  describe "calculateStats" $ do
    it "calculates mean duration correctly (in ms)" $ do
      let r1 = makeResult 10_000_000
      let r2 = makeResult 30_000_000
      let stats = calculateStats [r1, r2]
      meanMs stats `shouldBe` 20.0

    it "handles single response" $ do
      let r = makeResult 50_000_000
      let stats = calculateStats [r]
      meanMs stats `shouldBe` 50.0
      stdDevMs stats `shouldBe` 0.0
      p50Ms stats `shouldBe` 50.0
      p95Ms stats `shouldBe` 50.0
      p99Ms stats `shouldBe` 50.0

    it "handles empty response list" $ do
      let stats = calculateStats []
      totalRequests stats `shouldBe` 0
      countSuccess stats `shouldBe` 0
      meanMs stats `shouldBe` 0.0

    it "excludes failed responses from duration calculations" $ do
      let r1 = makeResult 10_000_000
      let r2 = makeErrorResult "Connection timeout"
      let r3 = makeResult 30_000_000
      let stats = calculateStats [r1, r2, r3]
      totalRequests stats `shouldBe` 3
      countSuccess stats `shouldBe` 2
      countFailure stats `shouldBe` 1
      meanMs stats `shouldBe` 20.0

    it "calculates correct percentiles for known data" $ do
      let responses = map (makeResult . (* 1_000_000)) [1 .. 10]
      let stats = calculateStats responses
      p50Ms stats `shouldSatisfy` (\x -> x > 5.0 && x < 6.0)
      p99Ms stats `shouldSatisfy` (> 9.0)

    it "calculates min and max correctly" $ do
      let responses = [makeResult 5_000_000, makeResult 100_000_000, makeResult 25_000_000]
      let stats = calculateStats responses
      minMs stats `shouldBe` 5.0
      maxMs stats `shouldBe` 100.0

  describe "variance / varianceList consistency" $ do
    it "agree on sample variance for the same data" $ do
      let xs = [10.0, 20.0, 30.0, 40.0, 50.0] :: [Double]
      let avg = sum xs / fromIntegral (length xs)
      let vecVar = variance (V.fromList xs)
      let listVar = varianceList avg xs
      abs (vecVar - listVar) `shouldSatisfy` (< 1e-10)

    it "both return 0 for a single element" $ do
      let xs = [42.0] :: [Double]
      variance (V.fromList xs) `shouldBe` 0
      varianceList 42.0 xs `shouldBe` 0

    it "both return 0 for empty input" $ do
      variance (V.fromList ([] :: [Double])) `shouldBe` 0
      varianceList 0.0 [] `shouldBe` 0
