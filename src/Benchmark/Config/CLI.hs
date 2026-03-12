-- | Command-line argument parsing for benchmark subcommands.
module Benchmark.Config.CLI
  ( parseArgs
  , Command (..)
  , BaselineMode (..)
  , commandParser
  )
where

import Benchmark.Types (ChartsSettings (..), OutputFormat (..))
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative

-- | Baseline operation mode.
data BaselineMode
  = NoBaseline
  | SaveBaseline Text
  | CompareBaseline Text
  | -- | Save as first, compare to second
    SaveAndCompare Text Text
  deriving (Show, Eq)

-- | Top-level CLI command (benchmark, compare, validate, etc.).
data Command
  = BenchmarkNway
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
      , chartsConfig :: Maybe ChartsSettings
      }
  | BenchmarkSingle
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
      , chartsConfig :: Maybe ChartsSettings
      }
  | Compare
      { compareFileA :: FilePath
      , compareFileB :: FilePath
      }
  | Validate
      { validateConfigPath :: FilePath
      , checkEndpoints :: Bool
      }
  deriving (Show, Eq)

-- | Parse command-line arguments into a 'Command'.
parseArgs :: IO Command
parseArgs = execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc ""
            <> header ""
        )

-- | Optparse-applicative parser for 'Command'.
commandParser :: Parser Command
commandParser =
  subparser
    ( command "benchmark-nway" (info benchmarkNwayOptions (progDesc "Run N-way benchmark comparison"))
        <> command "benchmark-single" (info benchmarkSingleOptions (progDesc "Run single-target benchmark"))
        <> command "compare" (info compareOptions (progDesc "Compare two saved benchmark results"))
        <> command "validate" (info validateOptions (progDesc "Validate config without running benchmarks"))
    )

configOption :: Parser FilePath
configOption =
  strOption
    ( long "config"
        <> short 'c'
        <> metavar "FILE"
        <> help "Path to the configuration file"
    )

saveBaselineOption :: Parser (Maybe Text)
saveBaselineOption =
  optional $
    T.pack
      <$> strOption
        ( long "save-baseline"
            <> metavar "NAME"
            <> help "Save results as a named baseline"
        )

compareBaselineOption :: Parser (Maybe Text)
compareBaselineOption =
  optional $
    T.pack
      <$> strOption
        ( long "compare-baseline"
            <> metavar "NAME"
            <> help "Compare against a named baseline (exit 1 on regression)"
        )

baselineModeParser :: Parser BaselineMode
baselineModeParser = combineBaseline <$> saveBaselineOption <*> compareBaselineOption
  where
    combineBaseline Nothing Nothing = NoBaseline
    combineBaseline (Just s) Nothing = SaveBaseline s
    combineBaseline Nothing (Just c) = CompareBaseline c
    combineBaseline (Just s) (Just c) = SaveAndCompare s c

outputFormatParser :: Parser OutputFormat
outputFormatParser = do
  let outputOption =
        optional $
          strOption
            ( long "output"
                <> metavar "FORMAT"
                <> help "Output format: 'markdown' to write a markdown report"
            )
  let reportPathOption =
        strOption
          ( long "report-path"
              <> metavar "FILE"
              <> help "Path for the markdown report (default: results/report.md)"
              <> value "results/report.md"
              <> showDefault
          )
  buildOutputFormat <$> outputOption <*> reportPathOption
  where
    buildOutputFormat :: Maybe String -> String -> OutputFormat
    buildOutputFormat (Just "markdown") path = OutputMarkdown path
    buildOutputFormat _ _ = OutputTerminal

chartsParser :: Parser (Maybe ChartsSettings)
chartsParser = optional $ buildCharts <$> chartsOption <*> chartsDirOption
  where
    chartsOption =
      strOption
        ( long "charts"
            <> metavar "TYPES"
            <> help "Chart types to generate (comma-separated, e.g. kde,cdf,violin or 'all')"
        )
    chartsDirOption =
      optional $
        strOption
          ( long "charts-dir"
              <> metavar "DIR"
              <> help "Output directory for charts (default: same as CSV output)"
          )
    buildCharts :: String -> Maybe FilePath -> ChartsSettings
    buildCharts types dir =
      ChartsSettings
        { chartsTypes = map T.pack $ splitOn ',' types
        , chartsDir = dir
        }
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn sep s =
      let (w, rest) = break (== sep) s
       in w : case rest of
            [] -> []
            (_ : rs) -> splitOn sep rs

benchmarkNwayOptions :: Parser Command
benchmarkNwayOptions =
  BenchmarkNway <$> configOption <*> baselineModeParser <*> outputFormatParser <*> chartsParser

benchmarkSingleOptions :: Parser Command
benchmarkSingleOptions =
  BenchmarkSingle <$> configOption <*> baselineModeParser <*> outputFormatParser <*> chartsParser

compareOptions :: Parser Command
compareOptions =
  Compare
    <$> strArgument (metavar "FILE_A" <> help "First benchmark result JSON file")
    <*> strArgument (metavar "FILE_B" <> help "Second benchmark result JSON file")

validateOptions :: Parser Command
validateOptions =
  Validate
    <$> configOption
    <*> switch (long "check-endpoints" <> help "Perform HTTP GET to each target's health check path")
