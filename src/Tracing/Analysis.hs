{-|
Module      : Tracing.Analysis
Description : Span duration aggregation
Stability   : experimental

Groups spans by name and computes duration statistics for trace analysis.
-}
module Tracing.Analysis
  ( aggregateBySpanName
  ) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Stats.Common qualified as Stats
import Tracing.Types (Milliseconds (..), Span (..), SpanAggregation (..), nsToMs)

-- | Group spans by name and compute duration statistics for each group.
aggregateBySpanName :: [Span] -> [SpanAggregation]
aggregateBySpanName spans =
  let grouped = groupByName spans
   in map computeAggregation (Map.toList grouped)

groupByName :: [Span] -> Map.Map Text [Span]
groupByName = foldr (\s -> Map.insertWith (++) (spanName s) [s]) Map.empty

computeAggregation :: (Text, [Span]) -> SpanAggregation
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
        , aggMinMs = Milliseconds $ head sorted
        , aggMaxMs = Milliseconds $ last sorted
        }
  where
    unMs (Milliseconds x) = x
