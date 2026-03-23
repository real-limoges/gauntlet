-- | Self-contained HTML reporter with inline SVG charts.
module Benchmark.Reporter.HTML (htmlReporter) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkStats (..)
  , MetricRegression (..)
  , PercentileComparison (..)
  , RegressionResult (..)
  , ValidationSummary (..)
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B

-- | Reporter that writes a self-contained HTML report to the given file path.
htmlReporter :: FilePath -> Reporter
htmlReporter path =
  Reporter
    { reportSingle = \targetUrl stats valids ->
        TIO.writeFile path $ buildHtml "Gauntlet Report" $ singleBody targetUrl stats valids
    , reportBenchmark = \namedStats pairs valids ->
        TIO.writeFile path $ buildHtml "Gauntlet Benchmark Report" $ benchmarkBody namedStats pairs valids
    , reportRegression = TIO.appendFile path . buildHtml "Gauntlet Regression Report" . regressionBody
    }

buildHtml :: B.Builder -> B.Builder -> Text
buildHtml title body =
  TL.toStrict $
    B.toLazyText $
      "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<title>"
        <> title
        <> "</title>\n"
        <> cssBlock
        <> "</head>\n<body>\n"
        <> body
        <> "</body>\n</html>\n"

cssBlock :: B.Builder
cssBlock =
  "<style>\n\
  \body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; \
  \max-width: 960px; margin: 2em auto; padding: 0 1em; color: #333; }\n\
  \h1 { border-bottom: 2px solid #2563eb; padding-bottom: 0.3em; }\n\
  \h2 { color: #1e40af; margin-top: 1.5em; }\n\
  \table { border-collapse: collapse; width: 100%; margin: 1em 0; }\n\
  \th, td { border: 1px solid #d1d5db; padding: 8px 12px; text-align: right; }\n\
  \th { background: #f3f4f6; text-align: left; }\n\
  \tr:nth-child(even) { background: #f9fafb; }\n\
  \.pass { color: #16a34a; font-weight: bold; }\n\
  \.fail { color: #dc2626; font-weight: bold; }\n\
  \.bar { fill: #3b82f6; }\n\
  \.bar-b { fill: #f59e0b; }\n\
  \svg { margin: 1em 0; }\n\
  \</style>\n"

-- Single target body
singleBody :: Text -> BenchmarkStats -> [ValidationSummary] -> B.Builder
singleBody targetUrl stats valids =
  "<h1>Benchmark: "
    <> esc targetUrl
    <> "</h1>\n"
    <> statsTable "Latency Statistics" stats
    <> svgBarChart stats
    <> validationSection valids

-- Benchmark comparison body
benchmarkBody ::
  Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> [ValidationSummary] -> B.Builder
benchmarkBody namedStats pairs valids =
  "<h1>Benchmark Comparison</h1>\n"
    <> mconcat [targetSection name s | (name, s) <- Map.toList namedStats]
    <> mconcat [comparisonSection a b comp | (a, b, comp) <- pairs]
    <> validationSection valids

targetSection :: Text -> BenchmarkStats -> B.Builder
targetSection name stats =
  "<h2>"
    <> esc name
    <> "</h2>\n"
    <> statsTable "Latency" stats
    <> svgBarChart stats

comparisonSection :: Text -> Text -> BayesianComparison -> B.Builder
comparisonSection nameA nameB comp =
  "<h2>"
    <> esc nameA
    <> " vs "
    <> esc nameB
    <> "</h2>\n"
    <> "<table>\n<tr><th>Metric</th><th>Value</th></tr>\n"
    <> row "P(B faster)" (showPct (probBFasterThanA comp))
    <> row "P(single request faster)" (showPct (probSingleRequestFaster comp))
    <> row "Cohen's d" (showD (effectSize comp))
    <> row "Mean difference (ms)" (showD (meanDifference comp))
    <> row "95% CI" (showD (credibleIntervalLower comp) <> " – " <> showD (credibleIntervalUpper comp))
    <> row "Relative effect" (showPct (relativeEffect comp / 100))
    <> pctRow "p95" (p95Comparison comp)
    <> pctRow "p99" (p99Comparison comp)
    <> "</table>\n"

pctRow :: B.Builder -> PercentileComparison -> B.Builder
pctRow label pc =
  row (label <> " diff (ms)") (showD (pctDifference pc))
    <> row (label <> " P(regression)") (showPct (probPctRegression pc))

-- Regression body
regressionBody :: RegressionResult -> B.Builder
regressionBody RegressionResult {..} =
  "<h1>Regression Report</h1>\n"
    <> "<p>Baseline: <strong>"
    <> esc regressionBaseline
    <> "</strong></p>\n"
    <> "<p>Result: "
    <> (if regressionPassed then "<span class=\"pass\">PASSED</span>" else "<span class=\"fail\">FAILED</span>")
    <> "</p>\n"
    <> "<table>\n<tr><th>Metric</th><th>Change</th><th>Threshold</th><th>Status</th></tr>\n"
    <> mconcat (map regressionRow regressionMetrics)
    <> "</table>\n"

regressionRow :: MetricRegression -> B.Builder
regressionRow MetricRegression {..} =
  "<tr><td style=\"text-align:left\">"
    <> esc metricName
    <> "</td>"
    <> "<td>"
    <> showPct metricChange
    <> "</td>"
    <> "<td>"
    <> showPct metricThreshold
    <> "</td>"
    <> "<td>"
    <> (if metricRegressed then "<span class=\"fail\">REGRESSED</span>" else "<span class=\"pass\">OK</span>")
    <> "</td></tr>\n"

-- Stats table
statsTable :: B.Builder -> BenchmarkStats -> B.Builder
statsTable title BenchmarkStats {..} =
  "<h3>"
    <> title
    <> "</h3>\n"
    <> "<table>\n<tr><th>Metric</th><th>Value</th></tr>\n"
    <> row "Requests" (showI totalRequests)
    <> row "Success" (showI countSuccess)
    <> row "Failure" (showI countFailure)
    <> row "Mean (ms)" (showD meanMs)
    <> row "Std Dev (ms)" (showD stdDevMs)
    <> row "Min (ms)" (showD minMs)
    <> row "Max (ms)" (showD maxMs)
    <> row "p50 (ms)" (showD p50Ms)
    <> row "p95 (ms)" (showD p95Ms)
    <> row "p99 (ms)" (showD p99Ms)
    <> row "ES (ms)" (showD esMs)
    <> "</table>\n"

row :: B.Builder -> B.Builder -> B.Builder
row label val =
  "<tr><td style=\"text-align:left\">" <> label <> "</td><td>" <> val <> "</td></tr>\n"

-- SVG bar chart for latency percentiles
svgBarChart :: BenchmarkStats -> B.Builder
svgBarChart BenchmarkStats {..} =
  let metrics = [("p50", p50Ms), ("p95", p95Ms), ("p99", p99Ms), ("ES", esMs)]
      maxVal = maximum (map snd metrics)
      barW = 80 -- pixels per bar
      barGap = 40 -- pixels between bars
      chartW = length metrics * (barW + barGap)
      chartH = 200 :: Int
      scale v = if maxVal <= 0 then 0 else round (v / maxVal * fromIntegral chartH) :: Int
   in "<svg width=\""
        <> showI chartW
        <> "\" height=\""
        <> showI (chartH + 40)
        <> "\" role=\"img\">\n"
        <> mconcat
          [ let h = scale v
                x = i * (barW + barGap) + 10
                y = chartH - h
             in "<rect class=\"bar\" x=\""
                  <> showI x
                  <> "\" y=\""
                  <> showI y
                  <> "\" width=\""
                  <> showI barW
                  <> "\" height=\""
                  <> showI h
                  <> "\"/>\n"
                  <> "<text x=\""
                  <> showI (x + barW `div` 2)
                  <> "\" y=\""
                  <> showI (chartH + 15)
                  <> "\" text-anchor=\"middle\" font-size=\"12\">"
                  <> B.fromString lbl
                  <> "</text>\n"
                  <> "<text x=\""
                  <> showI (x + barW `div` 2)
                  <> "\" y=\""
                  <> showI (y - 5)
                  <> "\" text-anchor=\"middle\" font-size=\"11\">"
                  <> showD1 v
                  <> "</text>\n"
          | (i, (lbl, v)) <- zip [0 :: Int ..] metrics
          ]
        <> "</svg>\n"

-- Validation section
validationSection :: [ValidationSummary] -> B.Builder
validationSection [] = mempty
validationSection valids =
  "<h2>Validation</h2>\n"
    <> "<table>\n<tr><th>#</th><th>Validated</th><th>Failed</th><th>Status</th></tr>\n"
    <> mconcat
      [ "<tr><td>"
          <> showI i
          <> "</td><td>"
          <> showI (totalValidated v)
          <> "</td><td>"
          <> showI (totalFailed v)
          <> "</td><td>"
          <> (if totalFailed v > 0 then "<span class=\"fail\">FAIL</span>" else "<span class=\"pass\">PASS</span>")
          <> "</td></tr>\n"
      | (i, v) <- zip [1 :: Int ..] valids
      ]
    <> "</table>\n"

-- Formatting helpers

showD :: Double -> B.Builder
showD = B.fromString . show

-- | Round to 1 decimal place.
showD1 :: Double -> B.Builder
showD1 d = B.fromString $ showFFloat' (1 :: Integer) d
  where
    showFFloat' n v =
      let factor = 10 ^ n :: Int
          rounded = fromIntegral (round (v * fromIntegral factor) :: Int) / fromIntegral factor :: Double
       in show rounded

showPct :: Double -> B.Builder
showPct d = B.fromString (show (fromIntegral (round (d * 1000) :: Int) / 10 :: Double)) <> "%"

showI :: Integral a => a -> B.Builder
showI = B.fromString . show . toInteger

-- | Escape HTML entities for safe embedding in markup.
esc :: Text -> B.Builder
esc = B.fromText . T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;"
