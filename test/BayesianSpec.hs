module BayesianSpec (bayesianSpec) where

import Benchmark.Types (BayesianComparison (..), BenchmarkStats (..))
import Stats.Benchmark (compareBayesian)
import Test.Hspec
import TestHelpers

bayesianSpec :: Spec
bayesianSpec = describe "Bayesian Comparison" $ do
  describe "compareBayesian" $ do
    it "detects obvious improvement (B faster)" $ do
      let statsA = mockStats 100.0 5.0
      let statsB = mockStats 50.0 5.0
      let result = compareBayesian statsA statsB
      probBFasterThanA result `shouldSatisfy` (> 0.99)

    it "detects obvious regression (A faster)" $ do
      let statsA = mockStats 50.0 5.0
      let statsB = mockStats 100.0 5.0
      let result = compareBayesian statsA statsB
      probBFasterThanA result `shouldSatisfy` (< 0.01)

    it "returns ~50% probability for identical distributions" $ do
      let statsA = mockStats 100.0 10.0
      let statsB = mockStats 100.0 10.0
      let result = compareBayesian statsA statsB
      probBFasterThanA result `shouldSatisfy` (\p -> p > 0.4 && p < 0.6)

    it "calculates positive mean difference when A is slower" $ do
      let statsA = mockStats 100.0 5.0
      let statsB = mockStats 80.0 5.0
      let result = compareBayesian statsA statsB
      meanDifference result `shouldBe` 20.0

    it "calculates negative mean difference when A is faster" $ do
      let statsA = mockStats 50.0 5.0
      let statsB = mockStats 80.0 5.0
      let result = compareBayesian statsA statsB
      meanDifference result `shouldBe` (-30.0)

    it "effect size (Cohen's d) is positive when A is slower" $ do
      let statsA = mockStats 100.0 10.0
      let statsB = mockStats 80.0 10.0
      let result = compareBayesian statsA statsB
      effectSize result `shouldSatisfy` (> 0)

    it "credible interval contains the true difference" $ do
      let statsA = mockStats 100.0 5.0
      let statsB = mockStats 90.0 5.0
      let result = compareBayesian statsA statsB
      let trueDiff = 10.0
      credibleIntervalLower result `shouldSatisfy` (< trueDiff)
      credibleIntervalUpper result `shouldSatisfy` (> trueDiff)

    it "relative effect is calculated correctly" $ do
      let statsA = mockStats 100.0 5.0
      let statsB = mockStats 80.0 5.0
      let result = compareBayesian statsA statsB
      relativeEffect result `shouldBe` 20.0

    it "returns finite results when both sides have zero successful requests" $ do
      let zeroCount = (mockStats 100.0 10.0) {countSuccess = 0}
      let result = compareBayesian zeroCount zeroCount
      probBFasterThanA result `shouldSatisfy` (\p -> not (isNaN p) && not (isInfinite p))
      probBFasterThanA result `shouldBe` 0.5

    it "returns finite results when one side has zero successful requests" $ do
      let statsA = mockStats 100.0 10.0
      let zeroCount = (mockStats 50.0 5.0) {countSuccess = 0}
      let result = compareBayesian statsA zeroCount
      probBFasterThanA result `shouldSatisfy` (\p -> not (isNaN p) && not (isInfinite p))
      credibleIntervalLower result `shouldSatisfy` (not . isNaN)
      credibleIntervalUpper result `shouldSatisfy` (not . isNaN)
