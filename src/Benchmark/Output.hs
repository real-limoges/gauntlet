{-|
Module      : Benchmark.Output
Description : CSV output for benchmark results
Stability   : experimental

Writes latency measurements to timestamped CSV files in the results directory.
-}
module Benchmark.Output
  ( initOutputFiles
  , writeLatencies
  , resultsDir
  ) where

import Benchmark.Types (Endpoint (..), Nanoseconds (..), TestingResponse (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (createDirectoryIfMissing)

-- | Directory where all benchmark output files are stored.
resultsDir :: FilePath
resultsDir = "results"

{-| Create the results directory and timestamped output files.
Returns a tuple of (csvFile, timestamp) for use by other outputs.
-}
initOutputFiles :: IO (FilePath, String)
initOutputFiles = do
  createDirectoryIfMissing True resultsDir
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getZonedTime
  let csvFile = resultsDir ++ "/latencies-" ++ timestamp ++ ".csv"
      logFile = resultsDir ++ "/failures-" ++ timestamp ++ ".log"
  writeFile logFile ""
  TIO.writeFile csvFile "payload_id,url,status_code,latency_ms\n"
  return (csvFile, timestamp)

writeLatencies :: FilePath -> [(Int, Endpoint, [TestingResponse])] -> IO ()
writeLatencies csvFile results = do
  let builder = foldMap formatResultBuilder results
  TLIO.appendFile csvFile (toLazyText builder)

formatResultBuilder :: (Int, Endpoint, [TestingResponse]) -> Builder
formatResultBuilder (idx, ep, responses) =
  foldMap (formatRow idx ep) responses

formatRow :: Int -> Endpoint -> TestingResponse -> Builder
formatRow idx ep r =
  mconcat
    [ B.fromString (show idx)
    , B.singleton ','
    , B.fromText (url ep)
    , B.singleton ','
    , B.fromString (show $ statusCode r)
    , B.singleton ','
    , B.fromString (show $ unNanoseconds $ durationNs r)
    , B.singleton '\n'
    ]
