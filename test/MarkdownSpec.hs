module MarkdownSpec (markdownSpec) where

import Benchmark.Report.Markdown
import Benchmark.Types
import Data.Aeson (toJSON)
import Data.Text qualified as T
import Test.Hspec
import TestHelpers (mockStats)

markdownSpec :: Spec
markdownSpec = describe "Benchmark.Report.Markdown" $ do
  describe "markdownSingleReport" $ do
    it "includes the label as a heading" $ do
      let report = markdownSingleReport "my-endpoint" (mockStats 50.0 5.0)
      report `shouldSatisfy` T.isInfixOf "my-endpoint"

    it "includes a Statistics section" $ do
      let report = markdownSingleReport "ep" (mockStats 100.0 10.0)
      report `shouldSatisfy` T.isInfixOf "### Statistics"

    it "includes p50, p95, p99 rows" $ do
      let report = markdownSingleReport "ep" (mockStats 100.0 10.0)
      report `shouldSatisfy` T.isInfixOf "p50"
      report `shouldSatisfy` T.isInfixOf "p95"
      report `shouldSatisfy` T.isInfixOf "p99"

    it "formats mean value as ms" $ do
      let report = markdownSingleReport "ep" (mockStats 42.5 2.0)
      report `shouldSatisfy` T.isInfixOf "42.50 ms"

  describe "markdownMultipleReport" $ do
    let primary = mockStats 100.0 10.0
        candidate = mockStats 80.0 8.0
        bayes = mockBayesianComparison
        report = markdownMultipleReport "primary-branch" "candidate-branch" primary candidate bayes

    it "includes both endpoint labels" $ do
      report `shouldSatisfy` T.isInfixOf "primary-branch"
      report `shouldSatisfy` T.isInfixOf "candidate-branch"

    it "includes Bayesian Analysis section" $ do
      report `shouldSatisfy` T.isInfixOf "Bayesian Analysis"

    it "includes probability of candidate being faster" $ do
      report `shouldSatisfy` T.isInfixOf "P(candidate faster"

    it "includes Cohen's d" $ do
      report `shouldSatisfy` T.isInfixOf "Cohen"

    it "includes credible interval" $ do
      report `shouldSatisfy` T.isInfixOf "credible interval"

    it "includes both stats sections" $ do
      report `shouldSatisfy` T.isInfixOf "#### primary-branch"
      report `shouldSatisfy` T.isInfixOf "#### candidate-branch"

  describe "markdownRegressionReport" $ do
    it "includes baseline name" $ do
      let result = mockRegressionResult "my-baseline" True []
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "my-baseline"

    it "shows PASSED status when no regression" $ do
      let result = mockRegressionResult "baseline" True []
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "PASSED"

    it "shows FAILED status when regression detected" $ do
      let m = MetricRegression "p99" 100.0 130.0 0.3 0.2 True
      let result = mockRegressionResult "baseline" False [m]
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "FAILED"

    it "includes metric table header" $ do
      let result = mockRegressionResult "baseline" True []
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "| Metric |"

    it "formats metric rows with PASS/FAIL" $ do
      let mPass = MetricRegression "p50" 50.0 52.0 0.04 0.2 False
      let mFail = MetricRegression "p99" 100.0 130.0 0.30 0.2 True
      let result = mockRegressionResult "baseline" False [mPass, mFail]
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "PASS"
      report `shouldSatisfy` T.isInfixOf "FAIL"

  describe "markdownValidationReport" $ do
    it "returns empty for empty list" $ do
      markdownValidationReport [] `shouldBe` T.empty

    it "includes Validation Results heading for non-empty input" $ do
      let s = ValidationSummary 10 2 []
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "Validation Results"

    it "includes counts in table rows" $ do
      let s = ValidationSummary 10 3 []
      let report = markdownValidationReport [s]
      -- 10 total, 7 passed, 3 failed
      report `shouldSatisfy` T.isInfixOf "10"
      report `shouldSatisfy` T.isInfixOf "7"
      report `shouldSatisfy` T.isInfixOf "3"

    it "includes validation errors when present" $ do
      let s = ValidationSummary 5 1 [StatusCodeMismatch 200 404]
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "Status code mismatch"

    it "describes FieldNotFound errors" $ do
      let s = ValidationSummary 5 1 [FieldNotFound "data.id"]
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "Field not found"
      report `shouldSatisfy` T.isInfixOf "data.id"

    it "describes FieldValueMismatch errors" $ do
      let s =
            ValidationSummary 5 1 [FieldValueMismatch "user.name" (toJSON ("alice" :: T.Text)) (toJSON ("bob" :: T.Text))]
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "Field value mismatch"
      report `shouldSatisfy` T.isInfixOf "user.name"

    it "describes BodyAbsent error" $ do
      let s = ValidationSummary 5 1 [BodyAbsent]
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "Response body absent"

    it "describes BodyInvalidJSON error" $ do
      let s = ValidationSummary 5 1 [BodyInvalidJSON]
      let report = markdownValidationReport [s]
      report `shouldSatisfy` T.isInfixOf "not valid JSON"

  describe "markdownMultipleReport with frequentist results" $ do
    let primary = mockStats 100.0 10.0
        candidate = mockStats 80.0 8.0

    it "renders MWU significant result" $ do
      let bayes = mockBayesianComparison {mannWhitneyU = Just (MWUResult True)}
      let report = markdownMultipleReport "p" "c" primary candidate bayes
      report `shouldSatisfy` T.isInfixOf "significant (p < 0.05)"

    it "renders MWU not significant result" $ do
      let bayes = mockBayesianComparison {mannWhitneyU = Just (MWUResult False)}
      let report = markdownMultipleReport "p" "c" primary candidate bayes
      report `shouldSatisfy` T.isInfixOf "not significant (p >= 0.05)"

    it "renders KS test with D statistic" $ do
      let bayes = mockBayesianComparison {kolmogorovSmirnov = Just (KSResult 0.42 0.03 True)}
      let report = markdownMultipleReport "p" "c" primary candidate bayes
      report `shouldSatisfy` T.isInfixOf "D = 0.420"
      report `shouldSatisfy` T.isInfixOf "significant"

    it "renders AD test with A-squared statistic" $ do
      let bayes = mockBayesianComparison {andersonDarling = Just (ADResult 2.5 0.01 True)}
      let report = markdownMultipleReport "p" "c" primary candidate bayes
      report `shouldSatisfy` T.isInfixOf "2.500"
      report `shouldSatisfy` T.isInfixOf "significant"

    it "renders 'sample too small' when frequentist tests are Nothing" $ do
      let report = markdownMultipleReport "p" "c" primary candidate mockBayesianComparison
      report `shouldSatisfy` T.isInfixOf "sample too small"

  describe "markdownRegressionReport with regressed metrics" $ do
    it "lists regressed metric names in summary" $ do
      let m = MetricRegression "p99" 100.0 130.0 0.3 0.15 True
      let result = mockRegressionResult "baseline" False [m]
      let report = markdownRegressionReport result
      report `shouldSatisfy` T.isInfixOf "Regressed metrics:"
      report `shouldSatisfy` T.isInfixOf "p99"

  describe "markdownVerifyReport" $ do
    it "includes Verification Report heading" $ do
      let ep = Endpoint "GET" "http://example.com/api" Nothing [] Nothing
      let report = markdownVerifyReport [(ep, [Match])]
      report `shouldSatisfy` T.isInfixOf "Verification Report"

    it "shows correct pass/fail counts" $ do
      let ep = Endpoint "POST" "http://example.com/api" Nothing [] Nothing
      let checks = [Match, Match, StatusMismatch 200 500]
      let report = markdownVerifyReport [(ep, checks)]
      report `shouldSatisfy` T.isInfixOf "3" -- samples
      report `shouldSatisfy` T.isInfixOf "2" -- passed
      report `shouldSatisfy` T.isInfixOf "1" -- failed
    it "renders body mismatch diffs" $ do
      let ep = Endpoint "GET" "http://example.com" Nothing [] Nothing
      let diff = JsonDiff "data.id" "123" "456"
      let report = markdownVerifyReport [(ep, [BodyMismatch [diff]])]
      report `shouldSatisfy` T.isInfixOf "data.id"
      report `shouldSatisfy` T.isInfixOf "123"
      report `shouldSatisfy` T.isInfixOf "456"

    it "renders empty results without crashing" $ do
      let report = markdownVerifyReport []
      report `shouldSatisfy` T.isInfixOf "Verification Report"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mockBayesianComparison :: BayesianComparison
mockBayesianComparison =
  BayesianComparison
    { probBFasterThanA = 0.87
    , probSingleRequestFaster = 0.72
    , meanDifference = 20.0
    , credibleIntervalLower = 15.0
    , credibleIntervalUpper = 25.0
    , effectSize = 0.5
    , relativeEffect = 0.20
    , p95Comparison = PercentileComparison 18.0 12.0 24.0 0.05
    , p99Comparison = PercentileComparison 22.0 14.0 30.0 0.08
    , mannWhitneyU = Nothing
    , kolmogorovSmirnov = Nothing
    , andersonDarling = Nothing
    }

mockRegressionResult :: T.Text -> Bool -> [MetricRegression] -> RegressionResult
mockRegressionResult name passed metrics =
  RegressionResult
    { regressionBaseline = name
    , regressionPassed = passed
    , regressionMetrics = metrics
    }
