-- | Tests for Benchmark.Reporter.Prometheus exposition format.
module PrometheusSpec (prometheusSpec) where

import Benchmark.Reporter.Prometheus (formatMetrics)
import Data.Text qualified as T
import TastyCompat (shouldBe, textShouldContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

prometheusSpec :: TestTree
prometheusSpec =
  testGroup
    "Benchmark.Reporter.Prometheus"
    [ testCase "formatMetrics produces TYPE and gauge lines" $ do
        let result = formatMetrics [("gauntlet", [("mean_ms", 50.0), ("p50_ms", 48.0)])]
        result `textShouldContain` "# TYPE gauntlet_mean_ms gauge"
        result `textShouldContain` "# TYPE gauntlet_p50_ms gauge"
        result `textShouldContain` "gauntlet_mean_ms 50.0"
        result `textShouldContain` "gauntlet_p50_ms 48.0"
    , testCase "formatMetrics sanitizes metric names" $ do
        let result = formatMetrics [("gauntlet", [("mean-ms", 50.0)])]
        result `textShouldContain` "gauntlet_mean_ms"
    , testCase "formatMetrics handles multiple groups" $ do
        let result =
              formatMetrics
                [ ("gauntlet_alpha", [("mean_ms", 50.0)])
                , ("gauntlet_beta", [("mean_ms", 60.0)])
                ]
        result `textShouldContain` "gauntlet_alpha_mean_ms 50.0"
        result `textShouldContain` "gauntlet_beta_mean_ms 60.0"
    , testCase "formatMetrics ends with newline" $ do
        let result = formatMetrics [("g", [("x", 1.0)])]
        T.last result `shouldBe` '\n'
    , testCase "formatMetrics empty input produces single newline" $ do
        let result = formatMetrics []
        result `shouldBe` "\n"
    ]
