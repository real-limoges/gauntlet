module Benchmark.Report.Output
  ( initOutputFiles
  , writeLatenciesWithTarget
  , resultsDir
  , formatRow
  , formatResultBuilder
  ) where

import Benchmark.Types (Endpoint (..), Nanoseconds (..), TestingResponse (..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing)

-- | Directory where all benchmark output files are stored.
resultsDir :: FilePath
resultsDir = "results"

-- | Create output files for a benchmark run.
initOutputFiles :: IO (FilePath, String)
initOutputFiles = do
  createDirectoryIfMissing True resultsDir
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getZonedTime
  let csvFile = resultsDir ++ "/latencies-" ++ timestamp ++ ".csv"
      logFile = resultsDir ++ "/failures-" ++ timestamp ++ ".log"
  writeFile logFile ""
  writeFile csvFile "target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso\n"
  return (csvFile, timestamp)

-- | Write latencies with a target name prefix.
writeLatenciesWithTarget :: FilePath -> Text -> [(Int, Endpoint, [TestingResponse])] -> IO ()
writeLatenciesWithTarget csvFile target results = do
  let builder = foldMap (formatResultBuilder target) results
  TLIO.appendFile csvFile (toLazyText builder)

formatResultBuilder :: Text -> (Int, Endpoint, [TestingResponse]) -> Builder
formatResultBuilder target (idx, ep, responses) =
  foldMap (formatRow target idx ep) responses

formatRow :: Text -> Int -> Endpoint -> TestingResponse -> Builder
formatRow target idx ep r =
  mconcat
    [ B.fromText target
    , B.singleton ','
    , B.fromString (show idx)
    , B.singleton ','
    , B.fromText (url ep)
    , B.singleton ','
    , B.fromText (method ep)
    , B.singleton ','
    , B.fromString (show $ statusCode r)
    , B.singleton ','
    , B.fromString (show (fromIntegral (unNanoseconds (durationNs r)) / 1_000_000 :: Double))
    , B.singleton ','
    , B.fromString (iso8601Show (requestedAt r))
    , B.singleton '\n'
    ]
