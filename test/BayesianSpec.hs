-- | Tests for Bayesian comparison in Stats.Benchmark.
module BayesianSpec (bayesianSpec) where

import Benchmark.Types (BayesianComparison (..), BenchmarkStats (..))
import Stats.Benchmark (compareBayesian)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (mockStats)

bayesianSpec :: TestTree
bayesianSpec =
  testGroup
    "Bayesian Comparison"
    [ testGroup
        "compareBayesian"
        [ testCase "detects obvious improvement (B faster)" $ do
            let statsA = mockStats 100.0 5.0
            let statsB = mockStats 50.0 5.0
            let result = compareBayesian statsA statsB
            probBFasterThanA result `shouldSatisfy` (> 0.99)
        , testCase "detects obvious regression (A faster)" $ do
            let statsA = mockStats 50.0 5.0
            let statsB = mockStats 100.0 5.0
            let result = compareBayesian statsA statsB
            probBFasterThanA result `shouldSatisfy` (< 0.01)
        , testCase "returns ~50% probability for identical distributions" $ do
            let statsA = mockStats 100.0 10.0
            let statsB = mockStats 100.0 10.0
            let result = compareBayesian statsA statsB
            probBFasterThanA result `shouldSatisfy` (\p -> p > 0.4 && p < 0.6)
        , testCase "calculates correct mean difference in both directions" $ do
            let resultPos = compareBayesian (mockStats 100.0 5.0) (mockStats 80.0 5.0)
            meanDifference resultPos `shouldBe` 20.0
            let resultNeg = compareBayesian (mockStats 50.0 5.0) (mockStats 80.0 5.0)
            meanDifference resultNeg `shouldBe` (-30.0)
        , testCase "effect size (Cohen's d) is positive when A is slower" $ do
            let statsA = mockStats 100.0 10.0
            let statsB = mockStats 80.0 10.0
            let result = compareBayesian statsA statsB
            effectSize result `shouldSatisfy` (> 0)
        , testCase "credible interval contains the true difference" $ do
            let statsA = mockStats 100.0 5.0
            let statsB = mockStats 90.0 5.0
            let result = compareBayesian statsA statsB
            let trueDiff = 10.0
            credibleIntervalLower result `shouldSatisfy` (< trueDiff)
            credibleIntervalUpper result `shouldSatisfy` (> trueDiff)
        , testCase "returns finite results when both sides have zero successful requests" $ do
            let zeroCount = (mockStats 100.0 10.0) {countSuccess = 0}
            let result = compareBayesian zeroCount zeroCount
            probBFasterThanA result `shouldSatisfy` (\p -> not (isNaN p) && not (isInfinite p))
            probBFasterThanA result `shouldBe` 0.5
        , testCase "returns finite results when one side has zero successful requests" $ do
            let statsA = mockStats 100.0 10.0
            let zeroCount = (mockStats 50.0 5.0) {countSuccess = 0}
            let result = compareBayesian statsA zeroCount
            probBFasterThanA result `shouldSatisfy` (\p -> not (isNaN p) && not (isInfinite p))
            credibleIntervalLower result `shouldSatisfy` (not . isNaN)
            credibleIntervalUpper result `shouldSatisfy` (not . isNaN)
        ]
    , testGroup
        "probBLessJittery"
        [ testCase "equal stddev returns ~0.5" $ do
            let result = compareBayesian (mockStats 100.0 10.0) (mockStats 100.0 10.0)
            probBLessJittery result `shouldSatisfy` (\p -> p > 0.4 && p < 0.6)
        , testCase "A more jittery than B returns >0.5" $ do
            let result = compareBayesian (mockStats 100.0 20.0) (mockStats 100.0 5.0)
            probBLessJittery result `shouldSatisfy` (> 0.5)
        , testCase "B more jittery than A returns <0.5" $ do
            let result = compareBayesian (mockStats 100.0 5.0) (mockStats 100.0 20.0)
            probBLessJittery result `shouldSatisfy` (< 0.5)
        , testCase "zero stddev returns 0.5" $ do
            let result = compareBayesian (mockStats 100.0 0.0) (mockStats 100.0 0.0)
            probBLessJittery result `shouldBe` 0.5
        , testCase "n=1 (countSuccess=1) returns 0.5" $ do
            let singleRequestStats = (mockStats 100.0 10.0) {countSuccess = 1}
            let result = compareBayesian singleRequestStats singleRequestStats
            probBLessJittery result `shouldBe` 0.5
        ]
    ]
