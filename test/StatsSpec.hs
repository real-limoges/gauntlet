-- | Tests for Stats.Benchmark (calculateStats).
module StatsSpec (statsSpec) where

import Benchmark.Types (BenchmarkStats (..))
import Data.Vector.Unboxed qualified as V
import Stats.Benchmark (calculateStats, earthMoversDistance)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.QuickCheck (NonEmptyList (NonEmpty), Positive (getPositive), ioProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
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
    , testGroup
        "earthMoversDistance"
        [ testCase "returns 0 for identical vectors" $ do
            let v = V.fromList [1.0, 2.0, 3.0]
            earthMoversDistance v v `shouldBe` 0
        , testCase "returns 1.0 for [0,0,0] vs [1,1,1]" $ do
            let a = V.fromList [0, 0, 0]
                b = V.fromList [1, 1, 1]
            earthMoversDistance a b `shouldBe` 1.0
        , testCase "returns positive result for unequal-size samples" $ do
            let a = V.fromList [1.0, 2.0, 3.0]
                b = V.fromList [4.0, 5.0]
            earthMoversDistance a b `shouldSatisfy` (> 0)
        , testCase "returns 0 for empty vs non-empty vectors" $ do
            earthMoversDistance V.empty (V.fromList [1]) `shouldBe` 0
            earthMoversDistance (V.fromList [1]) V.empty `shouldBe` 0
        , testProperty "earthMoversDistance a b >= 0 for non-empty vectors (QuickCheck)" $
            \(NonEmpty xs) (NonEmpty ys) -> ioProperty $ do
              let a = V.fromList (map getPositive xs :: [Double])
                  b = V.fromList (map getPositive ys :: [Double])
              earthMoversDistance a b `shouldSatisfy` (>= 0)
        , testProperty "earthMoversDistance a a == 0 for non-empty vectors (QuickCheck)" $
            \(NonEmpty xs) -> ioProperty $ do
              let a = V.fromList (map getPositive xs :: [Double])
              earthMoversDistance a a `shouldSatisfy` (<= 1e-10)
        ]
    ]
