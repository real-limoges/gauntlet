{-|
Module      : Tracing.Report
Description : Terminal output for trace analysis
Stability   : experimental

Formats and prints span duration statistics from trace analysis.
-}
module Tracing.Report
  ( printTraceAnalysis
  , printSpanTable
  , formatSpanRow
  , writeRawTraces
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Text.Printf (printf)
import Tracing.Analysis (aggregateBySpanName)
import Tracing.Types (SpanAggregation (..), Trace (..))

printTraceAnalysis :: [Trace] -> IO ()
printTraceAnalysis traces = do
  let allSpans = concatMap traceSpans traces
      aggregations = aggregateBySpanName allSpans

  putStrLn $ "Traces Analyzed: " ++ show (length traces)
  putStrLn $ "Total Spans: " ++ show (length allSpans)
  putStrLn ""
  printSpanTable aggregations

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
