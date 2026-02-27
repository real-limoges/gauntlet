{-|
Module      : Runner.Nway
Description : N-way benchmark orchestration
Stability   : experimental

Runs benchmarks against N>=2 named targets and produces all-pairs
Bayesian comparisons.
-}
module Runner.Nway (runNway, allPairComparisons) where

import Benchmark.Config (buildEndpoints)
import Benchmark.Output (initOutputFiles, writeMarkdownReport)
import Benchmark.Report (printNwayReport, printValidationSummary)
import Benchmark.Report.Markdown (markdownNwayReport, markdownValidationReport)
import Benchmark.Types
  ( BayesianComparison
  , BenchmarkStats
  , NamedTarget (..)
  , NwayConfig (..)
  , OutputFormat (..)
  , RunResult (..)
  , TestingResponse
  )
import Control.Monad (forM)
import Data.Text (Text)
import Data.Text qualified as T
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
    pure (targetName t, calculateStats timings, timings, validSummaries)

  let namedStats = [(name, stats) | (name, stats, _, _) <- results]
      pairInput = [(name, stats, raw) | (name, stats, raw, _) <- results]
      pairs = allPairComparisons pairInput
      validAll = concatMap (\(_, _, _, v) -> v) results

  printNwayReport namedStats pairs
  printValidationSummary validAll
  writeMarkdownReport outFmt $
    markdownNwayReport namedStats pairs
      <> markdownValidationReport validAll

  return RunSuccess

-- | Generate all N*(N-1)/2 pairwise Bayesian comparisons.
allPairComparisons :: [(Text, BenchmarkStats, [TestingResponse])] -> [(Text, Text, BayesianComparison)]
allPairComparisons [] = []
allPairComparisons ((nameA, statsA, timingsA) : rest) =
  [ (nameA, nameB, comparison)
  | (nameB, statsB, timingsB) <- rest
  , let comparison =
          addFrequentistTests
            timingsA
            timingsB
            (compareBayesian statsA statsB)
  ]
    ++ allPairComparisons rest
