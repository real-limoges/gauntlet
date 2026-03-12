-- | Terminal output formatting for distributed trace analysis results.
module Tracing.Report
  ( printTraceAnalysis
  , printSpanTable
  , formatSpanRow
  , writeRawTraces
  , aggregateBySpanName
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort, sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Stats.Common qualified as Stats
import Text.Printf (printf)
import Tracing.Types (Milliseconds (..), Span (..), SpanAggregation (..), Trace (..), nsToMs)

-- | Print a summary of all traces: count, total spans, and a span duration table.
printTraceAnalysis :: [Trace] -> IO ()
printTraceAnalysis traces = do
  let allSpans = concatMap traceSpans traces
      aggregations = aggregateBySpanName allSpans

  putStrLn $ "Traces Analyzed: " ++ show (length traces)
  putStrLn $ "Total Spans: " ++ show (length allSpans)
  putStrLn ""
  printSpanTable aggregations

-- | Print a table of span aggregations sorted by p95 latency (top 20).
printSpanTable :: [SpanAggregation] -> IO ()
printSpanTable [] = putStrLn "No spans to display."
printSpanTable aggs = do
  let sorted_aggs = sortBy (comparing (Down . aggP95Ms)) aggs
  putStrLn "#----- Span Duration Statistics -----#"
  putStrLn $
    printf
      "%-50s %6s %10s %10s %10s %10s"
      ("Span Name" :: String)
      ("Count" :: String)
      ("Mean" :: String)
      ("P50" :: String)
      ("P95" :: String)
      ("P99" :: String)
  putStrLn $ replicate 120 '-'
  mapM_ (putStrLn . formatSpanRow) (take 20 sorted_aggs)

-- | Format a single span aggregation as a fixed-width table row.
formatSpanRow :: SpanAggregation -> String
formatSpanRow SpanAggregation {..} =
  printf
    "%-50s %6d %9.2fms %9.2fms %9.2fms %9.2fms"
    (T.unpack $ T.take 50 aggSpanName)
    aggCount
    aggMeanMs
    aggP50Ms
    aggP95Ms
    aggP99Ms

-- | Write raw trace data to a JSON file for later inspection.
writeRawTraces :: FilePath -> [Trace] -> IO ()
writeRawTraces path traces = LBS.writeFile path (encode traces)

-- | Group spans by name and compute duration statistics for each group.
aggregateBySpanName :: [Span] -> [SpanAggregation]
aggregateBySpanName spans =
  let grouped = groupByName spans
   in map computeAggregation (Map.toList grouped)

groupByName :: [Span] -> Map.Map Text [Span]
groupByName = foldr (\s -> Map.insertWith (++) (spanName s) [s]) Map.empty

computeAggregation :: (Text, [Span]) -> SpanAggregation
computeAggregation (name, []) =
  SpanAggregation
    { aggSpanName = name
    , aggCount = 0
    , aggMeanMs = Milliseconds 0
    , aggStdDevMs = Milliseconds 0
    , aggP50Ms = Milliseconds 0
    , aggP95Ms = Milliseconds 0
    , aggP99Ms = Milliseconds 0
    , aggMinMs = Milliseconds 0
    , aggMaxMs = Milliseconds 0
    }
computeAggregation (name, spanList) =
  let durations = map (unMs . nsToMs . spanDurationNs) spanList
      sorted = sort durations
      n = length durations
      avg = sum durations / fromIntegral n
   in SpanAggregation
        { aggSpanName = name
        , aggCount = n
        , aggMeanMs = Milliseconds avg
        , aggStdDevMs = Milliseconds $ Stats.stdDevList avg sorted
        , aggP50Ms = Milliseconds $ Stats.percentileList 0.50 sorted
        , aggP95Ms = Milliseconds $ Stats.percentileList 0.95 sorted
        , aggP99Ms = Milliseconds $ Stats.percentileList 0.99 sorted
        , aggMinMs = Milliseconds $ minimum sorted
        , aggMaxMs = Milliseconds $ maximum sorted
        }
  where
    unMs (Milliseconds x) = x
