{- |
Module      : Tracing.Query
Description : TraceQL query builder
Stability   : experimental

Builds TraceQL queries for Grafana Tempo from query parameters.
See: https://grafana.com/docs/tempo/latest/traceql/
-}
module Tracing.Query (buildTraceQL)
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Tracing.Types (TraceQuery (..))

-- | Build a TraceQL query string from query parameters.
buildTraceQL :: TraceQuery -> Text
buildTraceQL tq =
    let conditions =
            catMaybes
                [ Just $ "resource.service.name=\"" <> queryService tq <> "\""
                , (\sn -> "name=\"" <> sn <> "\"") <$> querySpanName tq
                , ("duration>" <>) <$> queryMinDuration tq
                ]
     in "{" <> T.intercalate " && " conditions <> "}"
