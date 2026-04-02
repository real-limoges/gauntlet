-- | Prometheus Pushgateway reporter — pushes benchmark metrics in exposition format.
module Benchmark.Reporter.Prometheus (prometheusReporter, formatMetrics) where

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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Log (Logger, logInfo, logWarning)
import Network.HTTP.Client
  ( Manager
  , RequestBody (..)
  , httpNoBody
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseStatus
  )
import Network.HTTP.Types.Status (statusIsSuccessful)

-- | Reporter that pushes metrics to a Prometheus Pushgateway.
prometheusReporter :: Logger -> Manager -> Text -> Text -> Reporter
prometheusReporter logger mgr pushgatewayUrl jobName =
  Reporter
    { reportSingle = \targetUrl stats valids ->
        push (formatSingleMetrics targetUrl stats valids)
    , reportBenchmark = \namedStats pairs valids ->
        push (formatBenchmarkMetrics namedStats pairs valids)
    , reportRegression = \regression ->
        push (formatRegressionMetrics regression)
    }
  where
    push = pushMetrics logger mgr pushgatewayUrl jobName

-- | Push metrics to the Pushgateway via PUT.
pushMetrics :: Logger -> Manager -> Text -> Text -> Text -> IO ()
pushMetrics logger mgr pushgatewayUrl jobName body = do
  let url = T.unpack pushgatewayUrl <> "/metrics/job/" <> T.unpack jobName
  req0 <- parseRequest url
  let req =
        req0
          { method = "PUT"
          , requestBody = RequestBodyBS (TE.encodeUtf8 body)
          , requestHeaders = [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")]
          }
  resp <- httpNoBody req mgr
  let status = Network.HTTP.Client.responseStatus resp
  if statusIsSuccessful status
    then logInfo logger $ "Pushed metrics to " <> T.pack url
    else logWarning logger $ "Pushgateway returned " <> T.pack (show status) <> " for " <> T.pack url

-- | Format all metrics as Prometheus exposition text (visible for testing).
formatMetrics :: [(Text, [(Text, Double)])] -> Text
formatMetrics groups = T.intercalate "\n" (concatMap fmtGroup groups) <> "\n"
  where
    -- Each metric needs a # TYPE declaration followed by the gauge value
    fmtGroup (prefix, metrics) =
      [ "# TYPE " <> prefix <> "_" <> sanitize name <> " gauge"
      | (name, _) <- metrics
      ]
        <> [ prefix <> "_" <> sanitize name <> " " <> T.pack (show val)
           | (name, val) <- metrics
           ]

-- | Replace characters invalid in Prometheus metric names with underscores.
sanitize :: Text -> Text
sanitize = T.map (\c -> if c == '-' || c == '.' then '_' else c)

-- Single-target metrics
formatSingleMetrics :: Text -> BenchmarkStats -> [ValidationSummary] -> Text
formatSingleMetrics _targetUrl stats valids =
  formatMetrics $
    [("gauntlet", statsMetrics stats)]
      <> validationMetrics valids

-- Benchmark comparison metrics
formatBenchmarkMetrics ::
  Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> [ValidationSummary] -> Text
formatBenchmarkMetrics namedStats pairs valids =
  formatMetrics $
    [ ("gauntlet_" <> sanitize name, statsMetrics s)
    | (name, s) <- Map.toList namedStats
    ]
      <> [ ("gauntlet_comparison", comparisonMetrics a b comp)
         | (a, b, comp) <- pairs
         ]
      <> validationMetrics valids

-- Regression metrics
formatRegressionMetrics :: RegressionResult -> Text
formatRegressionMetrics RegressionResult {..} =
  formatMetrics
    [
      ( "gauntlet_regression"
      , ("passed", if regressionPassed then 1.0 else 0.0)
          : concatMap regressionMetricPairs regressionMetrics
      )
    ]

statsMetrics :: BenchmarkStats -> [(Text, Double)]
statsMetrics BenchmarkStats {..} =
  [ ("mean_ms", meanMs)
  , ("p50_ms", p50Ms)
  , ("p95_ms", p95Ms)
  , ("p99_ms", p99Ms)
  , ("es_ms", esMs)
  , ("stddev_ms", stdDevMs)
  , ("success_count", fromIntegral countSuccess)
  , ("failure_count", fromIntegral countFailure)
  ]

comparisonMetrics :: Text -> Text -> BayesianComparison -> [(Text, Double)]
comparisonMetrics _a _b BayesianComparison {..} =
  [ ("prob_b_faster", probBFasterThanA)
  , ("prob_single_request_faster", probSingleRequestFaster)
  , ("cohens_d", effectSize)
  , ("mean_diff_ms", meanDifference)
  ]
    ++ maybe [] (\d -> [("emd_ms", d)]) emd

validationMetrics :: [ValidationSummary] -> [(Text, [(Text, Double)])]
validationMetrics [] = []
validationMetrics valids =
  let total = sum (map totalValidated valids)
      failed = sum (map totalFailed valids)
   in [("gauntlet_validation", [("total", fromIntegral total), ("failed", fromIntegral failed)])]

regressionMetricPairs :: MetricRegression -> [(Text, Double)]
regressionMetricPairs MetricRegression {..} =
  [ (metricName <> "_change", metricChange)
  , (metricName <> "_threshold", metricThreshold)
  , (metricName <> "_regressed", if metricRegressed then 1.0 else 0.0)
  ]
