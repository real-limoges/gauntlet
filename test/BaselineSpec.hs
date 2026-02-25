module BaselineSpec (baselineSpec) where

import Benchmark.Baseline
import Benchmark.Types
import Data.List (find)
import Test.Hspec
import TestHelpers

baselineSpec :: Spec
baselineSpec = describe "Benchmark.Baseline" $ do
  describe "compareToBaseline" $ do
    it "passes when all metrics are within threshold" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 105.0 10.0
      let result = compareToBaseline defaultThresholds baseline current
      regressionPassed result `shouldBe` True
      regressionBaseline result `shouldBe` "v1.0"

    it "fails when mean exceeds threshold" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 115.0 10.0
      let result = compareToBaseline defaultThresholds baseline current
      regressionPassed result `shouldBe` False

    it "fails when p50 exceeds threshold" $ do
      let baseStats = mockStats 100.0 10.0
      let currStats = baseStats {p50Ms = 115.0}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      regressionPassed result `shouldBe` False

    it "fails when p95 exceeds threshold" $ do
      let baseStats = mockStats 100.0 10.0
      let currStats = baseStats {p95Ms = p95Ms baseStats * 1.15}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      regressionPassed result `shouldBe` False

    it "fails when p99 exceeds threshold (15%)" $ do
      let baseStats = mockStats 100.0 10.0
      let currStats = baseStats {p99Ms = p99Ms baseStats * 1.20}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      regressionPassed result `shouldBe` False

    it "passes when p99 is within higher threshold (15%)" $ do
      let baseStats = mockStats 100.0 10.0
      let currStats = baseStats {p99Ms = p99Ms baseStats * 1.10}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      regressionPassed result `shouldBe` True

    it "passes when metrics improve (negative change)" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 80.0 8.0
      let result = compareToBaseline defaultThresholds baseline current
      regressionPassed result `shouldBe` True

    it "returns four metric results" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 100.0 10.0
      let result = compareToBaseline defaultThresholds baseline current
      length (regressionMetrics result) `shouldBe` 4

    it "calculates correct percentage change" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 120.0 10.0
      let result = compareToBaseline defaultThresholds baseline current
      case find ((== "mean") . metricName) (regressionMetrics result) of
        Just meanMetric -> metricChange meanMetric `shouldSatisfy` (\c -> abs (c - 0.20) < 0.001)
        Nothing -> expectationFailure "Expected mean metric"

    it "marks only regressed metrics as regressed" $ do
      let baseStats = mockStats 100.0 10.0
      let currStats = baseStats {meanMs = 115.0}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      let regressedMetrics = filter metricRegressed (regressionMetrics result)
      length regressedMetrics `shouldBe` 1
      case regressedMetrics of
        [m] -> metricName m `shouldBe` "mean"
        _ -> expectationFailure "Expected exactly one regressed metric"

  describe "checkMetric edge cases (via compareToBaseline)" $ do
    it "handles zero baseline (current also zero)" $ do
      let baseStats = (mockStats 100.0 10.0) {meanMs = 0.0}
      let currStats = baseStats {meanMs = 0.0}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      case find ((== "mean") . metricName) (regressionMetrics result) of
        Just meanMetric -> do
          metricChange meanMetric `shouldBe` 0.0
          metricRegressed meanMetric `shouldBe` False
        Nothing -> expectationFailure "Expected mean metric"

    it "handles zero baseline (current non-zero)" $ do
      let baseStats = (mockStats 100.0 10.0) {meanMs = 0.0}
      let currStats = baseStats {meanMs = 50.0}
      let baseline = makeBaseline "v1.0" baseStats
      let result = compareToBaseline defaultThresholds baseline currStats
      case find ((== "mean") . metricName) (regressionMetrics result) of
        Just meanMetric -> do
          metricChange meanMetric `shouldBe` 1.0
          metricRegressed meanMetric `shouldBe` True
        Nothing -> expectationFailure "Expected mean metric"

    it "exact threshold boundary does not regress" $ do
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 110.0 10.0
      let result = compareToBaseline defaultThresholds baseline current
      case find ((== "mean") . metricName) (regressionMetrics result) of
        Just meanMetric -> metricRegressed meanMetric `shouldBe` False
        Nothing -> expectationFailure "Expected mean metric"

  describe "custom thresholds" $ do
    it "uses stricter thresholds correctly" $ do
      let strictThresholds = RegressionThresholds 0.05 0.05 0.05 0.05
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 108.0 10.0
      let result = compareToBaseline strictThresholds baseline current
      regressionPassed result `shouldBe` False

    it "uses looser thresholds correctly" $ do
      let looseThresholds = RegressionThresholds 0.25 0.25 0.25 0.25
      let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
      let current = mockStats 120.0 10.0
      let result = compareToBaseline looseThresholds baseline current
      regressionPassed result `shouldBe` True
