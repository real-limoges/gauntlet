-- | JUnit XML reporter for CI integration.
module Benchmark.Reporter.JUnit (junitReporter) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkStats (..)
  , MetricRegression (..)
  , RegressionResult (..)
  , ValidationSummary (..)
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B

-- | Reporter that writes JUnit XML to the given file path.
junitReporter :: FilePath -> Reporter
junitReporter path =
  Reporter
    { reportSingle = \targetUrl stats valids ->
        TIO.writeFile path $ buildJUnit $ singleSuite targetUrl stats valids
    , reportBenchmark = \namedStats pairs valids ->
        TIO.writeFile path $ buildJUnit $ benchmarkSuites namedStats pairs valids
    , reportRegression = TIO.appendFile path . buildJUnit . regressionSuite
    }

buildJUnit :: B.Builder -> Text
buildJUnit body =
  TL.toStrict $
    B.toLazyText $
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> body

-- | Build a single testsuite for a single-target benchmark.
singleSuite :: Text -> BenchmarkStats -> [ValidationSummary] -> B.Builder
singleSuite targetUrl stats valids =
  let metrics = statsTestCases stats
      validCases = validationTestCases valids
      allCases = metrics <> validCases
      failures = countFailures allCases
   in testsuite (B.fromText (esc targetUrl)) (length allCases) failures (mconcat (map renderTestCase allCases))

-- | Build testsuites for multi-target benchmark comparisons.
benchmarkSuites ::
  Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> [ValidationSummary] -> B.Builder
benchmarkSuites namedStats pairs valids =
  let statsSuites = map (uncurry statsSuite) (Map.toList namedStats)
      pairSuites = map pairSuite pairs
      validCases = validationTestCases valids
      validSuite
        | null validCases = mempty
        | otherwise =
            let failures = countFailures validCases
             in testsuite "validation" (length validCases) failures (mconcat (map renderTestCase validCases))
   in "<testsuites name=\"gauntlet\">\n"
        <> mconcat statsSuites
        <> mconcat pairSuites
        <> validSuite
        <> "</testsuites>\n"

-- | Testsuite for a single target's stats.
statsSuite :: Text -> BenchmarkStats -> B.Builder
statsSuite name stats =
  let cases = statsTestCases stats
   in testsuite (B.fromText (esc name)) (length cases) 0 (mconcat (map renderTestCase cases))

-- | Testsuite for a pairwise comparison.
pairSuite :: (Text, Text, BayesianComparison) -> B.Builder
pairSuite (nameA, nameB, comp) =
  let cases = comparisonTestCases nameA nameB comp
   in testsuite
        (B.fromText (esc nameA) <> " vs " <> B.fromText (esc nameB))
        (length cases)
        0
        (mconcat (map renderTestCase cases))

-- | Build a testsuite for a regression result.
regressionSuite :: RegressionResult -> B.Builder
regressionSuite RegressionResult {..} =
  let cases = map regressionTestCase regressionMetrics
      failures = countFailures cases
   in "<testsuites name=\"gauntlet-regression\">\n"
        <> testsuite
          ("baseline: " <> B.fromText (esc regressionBaseline))
          (length cases)
          failures
          (mconcat (map renderTestCase cases))
        <> "</testsuites>\n"

-- Test case data type
data TestCase = TestCase
  { tcName :: B.Builder
  , tcClassName :: B.Builder
  , tcFailure :: Maybe B.Builder
  }

countFailures :: [TestCase] -> Int
countFailures = length . filter (isJust . tcFailure)

renderTestCase :: TestCase -> B.Builder
renderTestCase TestCase {..} = case tcFailure of
  Nothing ->
    "    <testcase name=\"" <> tcName <> "\" classname=\"" <> tcClassName <> "\"/>\n"
  Just msg ->
    "    <testcase name=\""
      <> tcName
      <> "\" classname=\""
      <> tcClassName
      <> "\">\n"
      <> "      <failure message=\""
      <> tcName
      <> "\">"
      <> msg
      <> "</failure>\n"
      <> "    </testcase>\n"

-- | Stats as individual test cases.
statsTestCases :: BenchmarkStats -> [TestCase]
statsTestCases BenchmarkStats {..} =
  [ metric "mean_ms" (showD meanMs)
  , metric "p50_ms" (showD p50Ms)
  , metric "p95_ms" (showD p95Ms)
  , metric "p99_ms" (showD p99Ms)
  , metric "es_ms" (showD esMs)
  , metric "success_count" (showI countSuccess)
  , metric "failure_count" (showI countFailure)
  ]
  where
    metric name _val =
      TestCase
        { tcName = name
        , tcClassName = "gauntlet.stats"
        , tcFailure = Nothing
        }

-- | Bayesian comparison as test cases.
comparisonTestCases :: Text -> Text -> BayesianComparison -> [TestCase]
comparisonTestCases _nameA _nameB BayesianComparison {..} =
  [ tc "prob_b_faster" (showD probBFasterThanA)
  , tc "prob_single_request_faster" (showD probSingleRequestFaster)
  , tc "cohens_d" (showD effectSize)
  , tc "mean_diff_ms" (showD meanDifference)
  ]
  where
    tc name _val =
      TestCase
        { tcName = name
        , tcClassName = "gauntlet.comparison"
        , tcFailure = Nothing
        }

-- | Validation summaries as test cases.
validationTestCases :: [ValidationSummary] -> [TestCase]
validationTestCases = concatMap toCase . zip [1 :: Int ..]
  where
    toCase (idx, ValidationSummary {..}) =
      [ TestCase
          { tcName = "validation_" <> showI idx
          , tcClassName = "gauntlet.validation"
          , tcFailure =
              if totalFailed > 0
                then Just $ showI totalFailed <> " of " <> showI totalValidated <> " validations failed"
                else Nothing
          }
      ]

-- | A regression metric as a test case.
regressionTestCase :: MetricRegression -> TestCase
regressionTestCase MetricRegression {..} =
  TestCase
    { tcName = B.fromText (esc metricName)
    , tcClassName = "gauntlet.regression"
    , tcFailure =
        if metricRegressed
          then
            Just $
              "change=" <> showD (metricChange * 100) <> "% threshold=" <> showD (metricThreshold * 100) <> "%"
          else Nothing
    }

-- | Wrap test cases in a testsuite element.
testsuite :: B.Builder -> Int -> Int -> B.Builder -> B.Builder
testsuite name tests failures body =
  "  <testsuite name=\""
    <> name
    <> "\" tests=\""
    <> showI tests
    <> "\" failures=\""
    <> showI failures
    <> "\">\n"
    <> body
    <> "  </testsuite>\n"

-- Helpers
showD :: Double -> B.Builder
showD = B.fromString . show

showI :: Integral a => a -> B.Builder
showI = B.fromString . show . toInteger

-- | Escape XML special characters for safe embedding in attributes and text.
esc :: Text -> Text
esc = T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;"
