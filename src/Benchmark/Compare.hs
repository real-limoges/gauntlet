-- | Offline comparison of previously saved benchmark results.
module Benchmark.Compare (runCompare) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Types (BenchmarkStats, PerfTestError (..), RunResult (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Stats.Benchmark (compareBayesian)

-- | Load two JSON files containing BenchmarkStats and compare them offline.
runCompare :: Reporter -> FilePath -> FilePath -> IO RunResult
runCompare reporter fileA fileB = do
  resA <- eitherDecodeFileStrict @BenchmarkStats fileA
  resB <- eitherDecodeFileStrict @BenchmarkStats fileB
  case (resA, resB) of
    (Left err, _) -> do
      let msg = T.pack $ "failed to decode " <> fileA <> ": " <> err
      pure (RunError (ConfigParseError msg))
    (_, Left err) -> do
      let msg = T.pack $ "failed to decode " <> fileB <> ": " <> err
      pure (RunError (ConfigParseError msg))
    (Right statsA, Right statsB) -> do
      let nameA = T.pack fileA
          nameB = T.pack fileB
          cmp = compareBayesian statsA statsB
          statsMap = Map.fromList [(nameA, statsA), (nameB, statsB)]
          pairs = [(nameA, nameB, cmp)]
      reportBenchmark reporter statsMap pairs []
      pure RunSuccess
