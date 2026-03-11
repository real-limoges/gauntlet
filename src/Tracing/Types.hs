{-# LANGUAGE StrictData #-}

module Tracing.Types
  ( -- * Configuration
    TempoConfig (..)
  , TraceQuery (..)

    -- * Tempo API Response Types
  , TempoSearchResponse (..)
  , TraceMetadata (..)

    -- * Trace Data
  , Trace (..)
  , Span (..)
  , SpanKind (..)
  , SpanStatus (..)

    -- * Analysis Results
  , SpanAggregation (..)

    -- * Timing
  , nsToMs
  , Milliseconds (..)
  , Nanoseconds (..)
  ) where

import Benchmark.Types (Milliseconds (..), Nanoseconds (..), nsToMs)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Configuration for connecting to Grafana Tempo.
data TempoConfig = TempoConfig
  { tempoUrl :: Text
  -- ^ Base URL (e.g., "http://tempo:3200")
  , tempoServiceName :: Text
  -- ^ Service name to filter traces
  , tempoEnabled :: Bool
  , tempoAuthToken :: Maybe Text
  -- ^ Optional Bearer token
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Parameters for TraceQL search query.
data TraceQuery = TraceQuery
  { queryService :: Text
  -- ^ Service name filter (required)
  , querySpanName :: Maybe Text
  -- ^ Optional span name filter
  , queryStartNs :: Nanoseconds
  -- ^ Start of time range (nanoseconds since epoch)
  , queryEndNs :: Nanoseconds
  -- ^ End of time range (nanoseconds since epoch)
  , queryMinDuration :: Maybe Text
  -- ^ Optional minimum duration filter (e.g., "100ms")
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype TempoSearchResponse = TempoSearchResponse
  { foundTraces :: [TraceMetadata]
  }
  deriving stock (Show, Eq, Generic)

-- | Summary metadata from Tempo search results.
data TraceMetadata = TraceMetadata
  { metaTraceID :: Text
  , metaRootServiceName :: Text
  , metaRootTraceName :: Text
  , metaStartTimeUnixNano :: Nanoseconds
  , metaDurationMs :: Milliseconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Complete trace with all spans.
data Trace = Trace
  { traceId :: Text
  , traceSpans :: [Span]
  , traceTotalDurationNs :: Nanoseconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Individual span within a trace.
data Span = Span
  { spanId :: Text
  , spanParentId :: Maybe Text
  , spanName :: Text
  , spanServiceName :: Text
  , spanKind :: SpanKind
  , spanStartTimeNs :: Nanoseconds
  , spanEndTimeNs :: Nanoseconds
  , spanDurationNs :: Nanoseconds
  , spanStatus :: SpanStatus
  , spanAttributes :: Map Text Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SpanKind
  = SpanKindServer
  | SpanKindClient
  | SpanKindProducer
  | SpanKindConsumer
  | SpanKindInternal
  | SpanKindUnspecified
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SpanStatus
  = StatusOk
  | StatusError
  | StatusUnset
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

{-| Aggregated duration statistics for spans with the same name.
All duration values are in milliseconds.
-}
data SpanAggregation = SpanAggregation
  { aggSpanName :: Text
  , aggCount :: Int
  , aggMeanMs :: Milliseconds
  , aggStdDevMs :: Milliseconds
  , aggP50Ms :: Milliseconds
  , aggP95Ms :: Milliseconds
  , aggP99Ms :: Milliseconds
  , aggMinMs :: Milliseconds
  , aggMaxMs :: Milliseconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
