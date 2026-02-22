-- |
-- Module      : Runner.Tracing
-- Description : Optional Grafana Tempo trace fetching and reporting
module Runner.Tracing (runTraceAnalysis) where

import Benchmark.Output (resultsDir)
import Benchmark.Types (Settings (..), TempoSettings)
import Benchmark.Types qualified as PT
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Log (Logger, logInfo, logWarning)
import Network.HTTP.Client (Manager)
import Tracing.Client (fetchTracesForTimeRange)
import Tracing.Report (printTraceAnalysis, writeRawTraces)
import Tracing.Types (Trace, TraceQuery (..))
import Tracing.Types qualified as TT

buildTraceQuery :: TempoSettings -> TT.Nanoseconds -> TT.Nanoseconds -> TraceQuery
buildTraceQuery tempoSetts startNs endNs =
  TraceQuery
    { queryService = PT.tempoServiceName tempoSetts,
      querySpanName = Nothing,
      queryStartNs = startNs,
      queryEndNs = endNs,
      queryLimit = fromMaybe 100 (PT.tempoQueryLimit tempoSetts),
      queryMinDuration = Nothing
    }

toTempoConfig :: TempoSettings -> TT.TempoConfig
toTempoConfig ts =
  TT.TempoConfig
    { TT.tempoUrl = PT.tempoUrl ts,
      TT.tempoServiceName = PT.tempoServiceName ts,
      TT.tempoEnabled = fromMaybe True (PT.tempoEnabled ts),
      TT.tempoAuthToken = PT.tempoAuthToken ts
    }

-- | Fetch traces from Tempo, returning 'Right' traces or 'Left' an error message.
doFetchTraces :: Manager -> TempoSettings -> TT.Nanoseconds -> TT.Nanoseconds -> IO (Either Text [Trace])
doFetchTraces mgr tempoSetts startNs endNs = do
  let cfg = toTempoConfig tempoSetts
      query = buildTraceQuery tempoSetts startNs endNs
  result <- try (fetchTracesForTimeRange mgr cfg query) :: IO (Either SomeException (Either String [Trace]))
  return $ case result of
    Left err -> Left $ T.pack $ "Failed to fetch traces: " ++ show err
    Right (Left err) -> Left $ T.pack $ "Tempo error: " ++ err
    Right (Right []) -> Left "No traces found for time range"
    Right (Right traces) -> Right traces

-- | Fetch and print Tempo traces for the benchmark time window, if configured.
runTraceAnalysis :: Logger -> Manager -> Settings -> String -> TT.Nanoseconds -> TT.Nanoseconds -> IO ()
runTraceAnalysis logger mgr setts timestamp startNs endNs = case tempo setts of
  Nothing -> return ()
  Just tempoSetts
    | not (fromMaybe True (PT.tempoEnabled tempoSetts)) -> return ()
    | otherwise -> do
        logInfo logger "\n#----- Fetching Traces from Tempo -----#"
        fetchResult <- doFetchTraces mgr tempoSetts startNs endNs
        case fetchResult of
          Left err -> logWarning logger err
          Right traces -> do
            let tracesFile = resultsDir ++ "/traces-" ++ timestamp ++ ".json"
            writeRawTraces tracesFile traces
            logInfo logger $ T.pack $ "Raw traces written to: " ++ tracesFile
            printTraceAnalysis traces
