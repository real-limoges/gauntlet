-- | Reporter backend that generates latency distribution charts.
module Benchmark.Reporter.Plot (plotReporter) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Types.Config (ChartsSettings (..))
import Data.List (intercalate)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Reporter that generates charts by invoking the plot_latency.py script.
plotReporter :: ChartsSettings -> FilePath -> Reporter
plotReporter settings csvFile =
  Reporter
    { reportSingle = \_ _ _ -> generateCharts settings csvFile
    , reportNWay = \_ _ _ -> generateCharts settings csvFile
    , reportRegression = \_ -> pure ()
    }

generateCharts :: ChartsSettings -> FilePath -> IO ()
generateCharts ChartsSettings {..} csvFile = do
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
    ExitFailure code -> do
      hPutStrLn stderr $
        "Chart generation failed (exit " <> show code <> "): " <> stderrOut
