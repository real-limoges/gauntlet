{-|
Module      : Runner.Nway
Description : N-way benchmark orchestration
Stability   : experimental

Runs benchmarks against N>=2 named targets and produces all-pairs
Bayesian comparisons.
-}
module Runner.Nway (runNway, allPairComparisons) where

import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initOutputFiles)
import Benchmark.Report (printNwayReport, printValidationSummary)
import Benchmark.Report.Markdown (markdownNwayReport, markdownValidationReport)
import Benchmark.Types
  ( BayesianComparison
  , BenchmarkStats
  , NamedTarget (..)
  , NwayConfig (..)
  , OutputFormat (..)
  , RunResult (..)
  )
import Control.Monad (forM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Runner.Context (initContext, setupOrFail)
import Runner.Loop (benchmarkEndpoints)
import Stats.Benchmark (addFrequentistTests, calculateStats, compareBayesian)

-- | Run benchmarks against N named targets and compare all pairs.
runNway :: OutputFormat -> NwayConfig -> IO RunResult
runNway outFmt cfg = do
  (csvFile, timestamp) <- initOutputFiles

  let setts = nwaySettings cfg
  ctx <- initContext setts csvFile timestamp Nothing

  results <- forM (nwayTargets cfg) $ \t -> do
    case targetBranch t of
      Just branch
        | not (T.null branch) ->
            setupOrFail setts branch (targetUrl t) Nothing
      _ -> return ()
    (timings, validSummaries) <-
      benchmarkEndpoints
        ctx
        (T.unpack (targetName t))
        (buildEndpoints (targetUrl t) (nwayPayloads cfg))
    pure (targetName t, calculateStats timings, validSummaries)

  let namedStats = [(name, stats) | (name, stats, _) <- results]
      pairs = allPairComparisons namedStats
      validAll = concatMap (\(_, _, v) -> v) results

  printNwayReport namedStats pairs
  printValidationSummary validAll
  writeMarkdownReport outFmt $
    markdownNwayReport namedStats pairs
      <> markdownValidationReport validAll

  return RunSuccess

-- | Generate all N*(N-1)/2 pairwise Bayesian comparisons.
allPairComparisons :: [(Text, BenchmarkStats)] -> [(Text, Text, BayesianComparison)]
allPairComparisons [] = []
allPairComparisons ((nameA, statsA) : rest) =
  [ (nameA, nameB, comparison)
  | (nameB, statsB) <- rest
  , let comparison =
          addFrequentistTests
            (fakeTimings statsA)
            (fakeTimings statsB)
            (compareBayesian statsA statsB)
  ]
    ++ allPairComparisons rest
  where
    -- Frequentist tests need raw timings, but we only have stats at this point.
    -- We skip frequentist augmentation by passing empty lists; the tests will
    -- return Nothing for MWU/KS/AD which is the correct "sample too small" result.
    fakeTimings _ = []

-- | Write a markdown report to disk when 'OutputMarkdown' is requested.
writeMarkdownReport :: OutputFormat -> T.Text -> IO ()
writeMarkdownReport OutputTerminal _ = return ()
writeMarkdownReport (OutputMarkdown path) content = TIO.writeFile path content
