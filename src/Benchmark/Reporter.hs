-- | Reporter abstraction for composable benchmark output backends.
module Benchmark.Reporter
  ( Reporter (..)
  , noOpReporter
  , combineReporters
  , terminalReporter
  , markdownReporter
  , ciReporter
  , plotReporter
  , ReportingContext (..)
  )
where

import Benchmark.Config.CLI (BaselineMode)
import Benchmark.Report
  ( printBenchmarkReport
  , printRegressionResult
  , printSingleBenchmarkReport
  , printValidationSummary
  )
import Benchmark.Report.CI
  ( CIMode (..)
  , detectCIMode
  , formatForCI
  , writeArtifactReport
  , writeGitHubStepSummary
  )
import Benchmark.Report.Markdown
  ( markdownBenchmarkReport
  , markdownRegressionReport
  , markdownSingleReport
  , markdownValidationReport
  )
import Benchmark.Types
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Log (Logger, logWarning)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (readProcessWithExitCode)

-- | Record of callbacks for reporting benchmark results.
data Reporter = Reporter
  { reportSingle :: Text -> BenchmarkStats -> [ValidationSummary] -> IO ()
  -- ^ Called after a single-target benchmark completes.
  , reportBenchmark ::
      Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> [ValidationSummary] -> IO ()
  -- ^ Called after all targets complete with per-target stats and pairwise comparisons.
  , reportRegression :: RegressionResult -> IO ()
  -- ^ Called after baseline comparison when regressions are detected.
  }

-- | Bundles reporting dependencies that always travel together.
data ReportingContext = ReportingContext
  { rctxReporter :: Reporter
  , rctxBaselineMode :: BaselineMode
  , rctxLogger :: Logger
  }

-- | A reporter that discards all output.
noOpReporter :: Reporter
noOpReporter =
  Reporter
    { reportSingle = \_ _ _ -> pure ()
    , reportBenchmark = \_ _ _ -> pure ()
    , reportRegression = \_ -> pure ()
    }

-- | Sequence two reporters so both receive every event.
combineReporters :: [Reporter] -> Reporter
combineReporters rs =
  Reporter
    { reportSingle = \n s v -> mapM_ (\r -> reportSingle r n s v) rs
    , reportBenchmark = \m ps v -> mapM_ (\r -> reportBenchmark r m ps v) rs
    , reportRegression = \rr -> mapM_ (`reportRegression` rr) rs
    }

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

-- | Reporter for CI artifact generation. Detects CI mode and generates appropriate artifacts.
ciReporter :: IO Reporter
ciReporter = do
  ciMode <- detectCIMode
  pure $ case ciMode of
    None -> noOpReporter
    mode ->
      noOpReporter
        { reportRegression = \regression -> do
            formatForCI mode regression
            epoch <- (round <$> getPOSIXTime) :: IO Int
            let reportFile = "results/benchmark-report-" <> show epoch <> ".md"
            writeArtifactReport reportFile regression
            case mode of
              GitHub -> writeGitHubStepSummary regression
              _ -> pure ()
        }

-- | Reporter that generates charts by invoking the plot_latency.py script.
plotReporter :: Logger -> ChartsSettings -> FilePath -> Reporter
plotReporter logger settings csvFile =
  Reporter
    { reportSingle = \_ _ _ -> generateCharts logger settings csvFile
    , reportBenchmark = \_ _ _ -> generateCharts logger settings csvFile
    , reportRegression = \_ -> pure ()
    }

generateCharts :: Logger -> ChartsSettings -> FilePath -> IO ()
generateCharts logger ChartsSettings {..} csvFile = do
  let chartArg = intercalate "," (map T.unpack chartsTypes)
      outDir = case chartsDir of
        Just d -> d
        Nothing -> takeDirectory csvFile
      args =
        [ "run"
        , "--quiet"
        , "scripts/plot_latency.py"
        , csvFile
        , "--chart"
        , chartArg
        , "--output-dir"
        , outDir
        , "--quiet"
        ]
  (exitCode, _stdout, stderrOut) <- readProcessWithExitCode "uv" args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      logWarning logger $
        T.pack $
          "Chart generation failed (exit " <> show code <> "): " <> stderrOut
