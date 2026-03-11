module MarkdownSpec (markdownSpec) where

import Benchmark.Report.Markdown
import Benchmark.Types
import Data.Aeson (toJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Runner.Nway (allPairComparisons)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (mockBayesianComparison, mockStats)

markdownSpec :: TestTree
markdownSpec =
  testGroup
    "Benchmark.Report.Markdown"
    [ testGroup
        "markdownSingleReport"
        [ testCase "includes the label as a heading" $ do
            let report = markdownSingleReport "my-endpoint" (mockStats 50.0 5.0)
            report `shouldSatisfy` T.isInfixOf "my-endpoint"
        , testCase "includes a Statistics section" $ do
            let report = markdownSingleReport "ep" (mockStats 100.0 10.0)
            report `shouldSatisfy` T.isInfixOf "### Statistics"
        , testCase "includes p50, p95, p99 rows" $ do
            let report = markdownSingleReport "ep" (mockStats 100.0 10.0)
            report `shouldSatisfy` T.isInfixOf "p50"
            report `shouldSatisfy` T.isInfixOf "p95"
            report `shouldSatisfy` T.isInfixOf "p99"
        , testCase "formats mean value as ms" $ do
            let report = markdownSingleReport "ep" (mockStats 42.5 2.0)
            report `shouldSatisfy` T.isInfixOf "42.50 ms"
        ]
    , testGroup
        "markdownMultipleReport"
        [ let primary = mockStats 100.0 10.0
              candidate = mockStats 80.0 8.0
              bayes = mockBayesianComparison
              report = markdownMultipleReport "primary-branch" "candidate-branch" primary candidate bayes
           in testGroup
                "basic"
                [ testCase "includes both endpoint labels" $ do
                    report `shouldSatisfy` T.isInfixOf "primary-branch"
                    report `shouldSatisfy` T.isInfixOf "candidate-branch"
                , testCase "includes Bayesian Analysis section" $ do
                    report `shouldSatisfy` T.isInfixOf "Bayesian Analysis"
                , testCase "includes probability of candidate being faster" $ do
                    report `shouldSatisfy` T.isInfixOf "P(candidate faster"
                , testCase "includes Cohen's d" $ do
                    report `shouldSatisfy` T.isInfixOf "Cohen"
                , testCase "includes credible interval" $ do
                    report `shouldSatisfy` T.isInfixOf "credible interval"
                , testCase "includes both stats sections" $ do
                    report `shouldSatisfy` T.isInfixOf "#### primary-branch"
                    report `shouldSatisfy` T.isInfixOf "#### candidate-branch"
                ]
        ]
    , testGroup
        "markdownNwayReport"
        [ testCase "renders 2-target report with ranking and pair" $ do
            let namedStats = Map.fromList [("alpha", mockStats 10 1), ("beta", mockStats 20 2)]
                pairs = allPairComparisons (Map.toList namedStats)
                md = markdownNwayReport namedStats pairs
            md `shouldSatisfy` T.isInfixOf "Ranking"
            md `shouldSatisfy` T.isInfixOf "| # | Target |"
            md `shouldSatisfy` T.isInfixOf "alpha"
            md `shouldSatisfy` T.isInfixOf "beta"
            md `shouldSatisfy` T.isInfixOf "Bayesian Analysis"
        , testCase "renders 3-target report with all 3 pairs" $ do
            let namedStats =
                  Map.fromList
                    [ ("fast", mockStats 10 1)
                    , ("medium", mockStats 50 5)
                    , ("slow", mockStats 100 10)
                    ]
                pairs = allPairComparisons (Map.toList namedStats)
                md = markdownNwayReport namedStats pairs
            -- All 3 target names present
            md `shouldSatisfy` T.isInfixOf "fast"
            md `shouldSatisfy` T.isInfixOf "medium"
            md `shouldSatisfy` T.isInfixOf "slow"
            -- Should have 3 pairwise comparison sections (each includes "---")
            let separatorCount = length (filter (== "---") (T.lines md))
            separatorCount `shouldBe` 3
        ]
    , testGroup
        "markdownRegressionReport"
        [ testCase "includes baseline name" $ do
            let result = mockRegressionResult "my-baseline" True []
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "my-baseline"
        , testCase "shows PASSED status when no regression" $ do
            let result = mockRegressionResult "baseline" True []
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "PASSED"
        , testCase "shows FAILED status when regression detected" $ do
            let m = MetricRegression "p99" 100.0 130.0 0.3 0.2 True
            let result = mockRegressionResult "baseline" False [m]
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "FAILED"
        , testCase "includes metric table header" $ do
            let result = mockRegressionResult "baseline" True []
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "| Metric |"
        , testCase "formats metric rows with PASS/FAIL" $ do
            let mPass = MetricRegression "p50" 50.0 52.0 0.04 0.2 False
            let mFail = MetricRegression "p99" 100.0 130.0 0.30 0.2 True
            let result = mockRegressionResult "baseline" False [mPass, mFail]
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "PASS"
            report `shouldSatisfy` T.isInfixOf "FAIL"
        , testCase "pass result contains 'All metrics within'" $ do
            let mPass = MetricRegression "mean" 50.0 52.0 0.04 0.10 False
            let result = mockRegressionResult "baseline" True [mPass]
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "All metrics within acceptable thresholds"
        , testCase "fail result lists regressed metric names" $ do
            let m1 = MetricRegression "p95" 80.0 100.0 0.25 0.10 True
            let m2 = MetricRegression "p99" 100.0 140.0 0.40 0.15 True
            let result = mockRegressionResult "baseline" False [m1, m2]
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "Regressed metrics:"
            report `shouldSatisfy` T.isInfixOf "p95"
            report `shouldSatisfy` T.isInfixOf "p99"
        ]
    , testGroup
        "markdownValidationReport"
        [ testCase "returns empty for empty list" $ do
            markdownValidationReport [] `shouldBe` T.empty
        , testCase "includes Validation Results heading for non-empty input" $ do
            let s = ValidationSummary 10 2 []
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "Validation Results"
        , testCase "includes counts in table rows" $ do
            let s = ValidationSummary 10 3 []
            let report = markdownValidationReport [s]
            -- 10 total, 7 passed, 3 failed
            report `shouldSatisfy` T.isInfixOf "10"
            report `shouldSatisfy` T.isInfixOf "7"
            report `shouldSatisfy` T.isInfixOf "3"
        , testCase "includes validation errors when present" $ do
            let s = ValidationSummary 5 1 [StatusCodeMismatch 200 404]
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "Status code mismatch"
        , testCase "describes FieldNotFound errors" $ do
            let s = ValidationSummary 5 1 [FieldNotFound "data.id"]
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "Field not found"
            report `shouldSatisfy` T.isInfixOf "data.id"
        , testCase "describes FieldValueMismatch errors" $ do
            let s =
                  ValidationSummary 5 1 [FieldValueMismatch "user.name" (toJSON ("alice" :: T.Text)) (toJSON ("bob" :: T.Text))]
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "Field value mismatch"
            report `shouldSatisfy` T.isInfixOf "user.name"
        , testCase "describes BodyAbsent error" $ do
            let s = ValidationSummary 5 1 [BodyAbsent]
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "Response body absent"
        , testCase "describes BodyInvalidJSON error" $ do
            let s = ValidationSummary 5 1 [BodyInvalidJSON]
            let report = markdownValidationReport [s]
            report `shouldSatisfy` T.isInfixOf "not valid JSON"
        , testCase "renders multiple validation summaries" $ do
            let s1 = ValidationSummary 10 0 []
                s2 = ValidationSummary 5 2 [FieldNotFound "$.x"]
            let report = markdownValidationReport [s1, s2]
            report `shouldSatisfy` T.isInfixOf "Field not found"
        ]
    , testGroup
        "markdownRegressionReport with regressed metrics"
        [ testCase "lists regressed metric names in summary" $ do
            let m = MetricRegression "p99" 100.0 130.0 0.3 0.15 True
            let result = mockRegressionResult "baseline" False [m]
            let report = markdownRegressionReport result
            report `shouldSatisfy` T.isInfixOf "Regressed metrics:"
            report `shouldSatisfy` T.isInfixOf "p99"
        ]
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mockRegressionResult :: T.Text -> Bool -> [MetricRegression] -> RegressionResult
mockRegressionResult name passed metrics =
  RegressionResult
    { regressionBaseline = name
    , regressionPassed = passed
    , regressionMetrics = metrics
    }
