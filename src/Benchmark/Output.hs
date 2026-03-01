{-|
Module      : Benchmark.Output
Description : CSV output for benchmark results
Stability   : experimental

Writes latency measurements to timestamped CSV files in the results directory.
-}
module Benchmark.Output
  ( initOutputFiles
  , initNwayOutputFiles
  , writeLatencies
  , writeLatenciesWithTarget
  , writeMarkdownReport
  , resultsDir
  , formatRow
  , formatResultBuilder
  ) where

import Benchmark.Types (Endpoint (..), Nanoseconds (..), OutputFormat (..), TestingResponse (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (createDirectoryIfMissing)

-- | Directory where all benchmark output files are stored.
resultsDir :: FilePath
resultsDir = "results"

-- | Create output files for a standard (non-N-way) run.
initOutputFiles :: IO (FilePath, String)
initOutputFiles = initOutputFilesWith "payload_id,url,status_code,latency_ms\n"

-- | Create output files with N-way CSV header (includes target_name column).
initNwayOutputFiles :: IO (FilePath, String)
initNwayOutputFiles = initOutputFilesWith "target_name,payload_id,url,status_code,latency_ms\n"

initOutputFilesWith :: String -> IO (FilePath, String)
initOutputFilesWith csvHeader = do
  createDirectoryIfMissing True resultsDir
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getZonedTime
  let csvFile = resultsDir ++ "/latencies-" ++ timestamp ++ ".csv"
      logFile = resultsDir ++ "/failures-" ++ timestamp ++ ".log"
  writeFile logFile ""
  writeFile csvFile csvHeader
  return (csvFile, timestamp)

writeLatencies :: FilePath -> [(Int, Endpoint, [TestingResponse])] -> IO ()
writeLatencies csvFile = writeLatenciesWithTarget csvFile Nothing

-- | Write latencies with an optional target name prefix for N-way runs.
writeLatenciesWithTarget :: FilePath -> Maybe Text -> [(Int, Endpoint, [TestingResponse])] -> IO ()
writeLatenciesWithTarget csvFile mTarget results = do
  let builder = foldMap (formatResultWithTarget mTarget) results
  TLIO.appendFile csvFile (toLazyText builder)

-- | Write a markdown report to disk when 'OutputMarkdown' is requested.
writeMarkdownReport :: OutputFormat -> Text -> IO ()
writeMarkdownReport OutputTerminal _ = return ()
writeMarkdownReport (OutputMarkdown path) content = TIO.writeFile path content

formatResultBuilder :: (Int, Endpoint, [TestingResponse]) -> Builder
formatResultBuilder = formatResultWithTarget Nothing

formatResultWithTarget :: Maybe Text -> (Int, Endpoint, [TestingResponse]) -> Builder
formatResultWithTarget mTarget (idx, ep, responses) =
  foldMap (formatRowWithTarget mTarget idx ep) responses

formatRow :: Int -> Endpoint -> TestingResponse -> Builder
formatRow = formatRowWithTarget Nothing

formatRowWithTarget :: Maybe Text -> Int -> Endpoint -> TestingResponse -> Builder
formatRowWithTarget mTarget idx ep r =
  mconcat $
    maybe [] (\t -> [B.fromText t, B.singleton ',']) mTarget
      ++ [ B.fromString (show idx)
         , B.singleton ','
         , B.fromText (url ep)
         , B.singleton ','
         , B.fromString (show $ statusCode r)
         , B.singleton ','
         , B.fromString (show $ unNanoseconds $ durationNs r)
         , B.singleton '\n'
         ]
