-- | Reporter backend that writes markdown reports to a file.
module Benchmark.Reporter.Markdown (markdownReporter) where

import Benchmark.Report.Markdown
  ( markdownBenchmarkReport
  , markdownRegressionReport
  , markdownSingleReport
  , markdownValidationReport
  )
import Benchmark.Reporter (Reporter (..))
import Data.Text.IO qualified as TIO

-- | Reporter that writes markdown output to the given file path.
markdownReporter :: FilePath -> Reporter
markdownReporter path =
  Reporter
    { reportSingle = \targetUrl stats valids ->
        TIO.writeFile path $
          markdownSingleReport targetUrl stats
            <> markdownValidationReport valids
    , reportBenchmark = \namedStats pairs valids ->
        TIO.writeFile path $
          markdownBenchmarkReport namedStats pairs
            <> markdownValidationReport valids
    , reportRegression = TIO.appendFile path . markdownRegressionReport
    }
