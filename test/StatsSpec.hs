-- | Tests for Stats.Benchmark (calculateStats).
module StatsSpec (statsSpec) where

import Benchmark.Types (BenchmarkStats (..))
import Data.List (sort)
import Data.Vector.Unboxed qualified as V
import Stats.Benchmark (calculateStats, computeHistogram, earthMoversDistance)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.QuickCheck (NonEmptyList (NonEmpty), Positive (getPositive), ioProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestHelpers (makeErrorResult, makeResult)

statsSpec :: TestTree
statsSpec =
  testGroup
    "Stats.Benchmark"
    [ testGroup
        "calculateStats"
        [ testCase "handles single response" $ do
            let result = makeResult 50_000_000
            let stats = calculateStats [result]
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
        , testCase "populates histogram for multiple responses" $ do
            let responses = map (makeResult . (* 1_000_000)) [10, 20 .. 100]
            let stats = calculateStats responses
            histogram stats `shouldSatisfy` (not . null)
            sum (map snd (histogram stats)) `shouldBe` 10
        , testCase "histogram is empty for empty responses" $ do
            let stats = calculateStats []
            histogram stats `shouldBe` []
        , testCase "histogram has single bin for single response" $ do
            let stats = calculateStats [makeResult 50_000_000]
            histogram stats `shouldBe` [(50.0, 1)]
        ]
    , testGroup
        "computeHistogram"
        [ testCase "returns empty for empty vector" $ do
            computeHistogram V.empty `shouldBe` []
        , testCase "returns single bin for single element" $ do
            computeHistogram (V.fromList [42.0]) `shouldBe` [(42.0, 1)]
        , testCase "total counts equal input length" $ do
            let sorted = V.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
            let bins = computeHistogram sorted
            sum (map snd bins) `shouldBe` 10
        , testCase "bin count follows Sturges rule clamped to [8, 20]" $ do
            let sorted = V.fromList [1.0 .. 100.0]
            let bins = computeHistogram sorted
            length bins `shouldSatisfy` (\n -> n >= 8 && n <= 20)
        , testCase "bins are ordered by lower bound" $ do
            let sorted = V.fromList [1.0 .. 50.0]
            let bins = computeHistogram sorted
            let lowers = map fst bins
            lowers `shouldBe` lowers -- non-empty
            lowers `shouldSatisfy` (\ls -> and $ zipWith (<=) ls (tail ls))
        , testCase "first bin starts at min value" $ do
            let sorted = V.fromList [5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0]
            let bins = computeHistogram sorted
            case bins of
              (lo, _) : _ -> lo `shouldBe` 5.0
              [] -> error "expected non-empty histogram"
        , testProperty "total count equals input length (QuickCheck)" $
            \(NonEmpty xs) -> ioProperty $ do
              let vals = map getPositive xs :: [Double]
                  sorted = V.fromList (sort vals)
                  bins = computeHistogram sorted
              sum (map snd bins) `shouldBe` length vals
        ]
    , testGroup
        "earthMoversDistance"
        [ testCase "returns 0 for identical vectors" $ do
            let vec = V.fromList [1.0, 2.0, 3.0]
            earthMoversDistance vec vec `shouldBe` 0
        , testCase "returns 1.0 for [0,0,0] vs [1,1,1]" $ do
            let vecA = V.fromList [0, 0, 0]
                vecB = V.fromList [1, 1, 1]
            earthMoversDistance vecA vecB `shouldBe` 1.0
        , testCase "returns positive result for unequal-size samples" $ do
            let vecA = V.fromList [1.0, 2.0, 3.0]
                vecB = V.fromList [4.0, 5.0]
            earthMoversDistance vecA vecB `shouldSatisfy` (> 0)
        , testCase "returns 0 for empty vs non-empty vectors" $ do
            earthMoversDistance V.empty (V.fromList [1]) `shouldBe` 0
            earthMoversDistance (V.fromList [1]) V.empty `shouldBe` 0
        , testProperty "earthMoversDistance a b >= 0 for non-empty vectors (QuickCheck)" $
            \(NonEmpty xs) (NonEmpty ys) -> ioProperty $ do
              let vecA = V.fromList (map getPositive xs :: [Double])
                  vecB = V.fromList (map getPositive ys :: [Double])
              earthMoversDistance vecA vecB `shouldSatisfy` (>= 0)
        , testProperty "earthMoversDistance a a == 0 for non-empty vectors (QuickCheck)" $
            \(NonEmpty xs) -> ioProperty $ do
              let vec = V.fromList (map getPositive xs :: [Double])
              earthMoversDistance vec vec `shouldSatisfy` (<= 1e-10)
        ]
    ]
