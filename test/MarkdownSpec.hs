module MarkdownSpec (markdownSpec) where

import Benchmark.Report.Markdown
import Benchmark.Types
import Data.Aeson (toJSON)
import Data.Text qualified as T
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
        ]
    , testGroup
        "markdownMultipleReport with frequentist results"
        [ let primary = mockStats 100.0 10.0
              candidate = mockStats 80.0 8.0
           in testGroup
                "frequentist"
                [ testCase "renders MWU significant result" $ do
                    let bayes = mockBayesianComparison {mannWhitneyU = Just (MWUResult True)}
                    let report = markdownMultipleReport "p" "c" primary candidate bayes
                    report `shouldSatisfy` T.isInfixOf "significant (p < 0.05)"
                , testCase "renders MWU not significant result" $ do
                    let bayes = mockBayesianComparison {mannWhitneyU = Just (MWUResult False)}
                    let report = markdownMultipleReport "p" "c" primary candidate bayes
                    report `shouldSatisfy` T.isInfixOf "not significant (p >= 0.05)"
                , testCase "renders KS test with D statistic" $ do
                    let bayes = mockBayesianComparison {kolmogorovSmirnov = Just (KSResult 0.42 0.03 True)}
                    let report = markdownMultipleReport "p" "c" primary candidate bayes
                    report `shouldSatisfy` T.isInfixOf "D = 0.420"
                    report `shouldSatisfy` T.isInfixOf "significant"
                , testCase "renders AD test with A-squared statistic" $ do
                    let bayes = mockBayesianComparison {andersonDarling = Just (ADResult 2.5 0.01 True)}
                    let report = markdownMultipleReport "p" "c" primary candidate bayes
                    report `shouldSatisfy` T.isInfixOf "2.500"
                    report `shouldSatisfy` T.isInfixOf "significant"
                , testCase "renders 'sample too small' when frequentist tests are Nothing" $ do
                    let report = markdownMultipleReport "p" "c" primary candidate mockBayesianComparison
                    report `shouldSatisfy` T.isInfixOf "sample too small"
                ]
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
    , testGroup
        "markdownVerifyReport"
        [ testCase "includes Verification Report heading" $ do
            let ep = Endpoint "GET" "http://example.com/api" Nothing [] Nothing
            let report = markdownVerifyReport [(ep, [Match])]
            report `shouldSatisfy` T.isInfixOf "Verification Report"
        , testCase "shows correct pass/fail counts" $ do
            let ep = Endpoint "POST" "http://example.com/api" Nothing [] Nothing
            let checks = [Match, Match, StatusMismatch 200 500]
            let report = markdownVerifyReport [(ep, checks)]
            report `shouldSatisfy` T.isInfixOf "3" -- samples
            report `shouldSatisfy` T.isInfixOf "2" -- passed
            report `shouldSatisfy` T.isInfixOf "1" -- failed
        , testCase "renders body mismatch diffs" $ do
            let ep = Endpoint "GET" "http://example.com" Nothing [] Nothing
            let diff = JsonDiff "data.id" "123" "456"
            let report = markdownVerifyReport [(ep, [BodyMismatch [diff]])]
            report `shouldSatisfy` T.isInfixOf "data.id"
            report `shouldSatisfy` T.isInfixOf "123"
            report `shouldSatisfy` T.isInfixOf "456"
        , testCase "renders empty results without crashing" $ do
            let report = markdownVerifyReport []
            report `shouldSatisfy` T.isInfixOf "Verification Report"
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
