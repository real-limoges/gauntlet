-- | Reporter backend for terminal output.
module Benchmark.Reporter.Terminal
  ( terminalReporter
  ) where

import Benchmark.Report (printBenchmarkReport, printSingleBenchmarkReport, printValidationSummary)
import Benchmark.Report.Baseline (printRegressionResult)
import Benchmark.Reporter (Reporter (..))

-- | Reporter that prints benchmark results, validation summaries, and regression reports to the terminal.
terminalReporter :: Reporter
terminalReporter =
  Reporter
    { reportSingle = \targetUrl stats valids -> do
        printSingleBenchmarkReport targetUrl stats
        printValidationSummary valids
    , reportBenchmark = \namedStats pairs valids -> do
        printBenchmarkReport namedStats pairs
        printValidationSummary valids
    , reportRegression = printRegressionResult
    }
