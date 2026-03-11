module Benchmark.Config.CLI
  ( parseArgs
  , Command (..)
  , BaselineMode (..)
  , commandParser
  )
where

import Benchmark.Types (OutputFormat (..))
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

data Command
  = BenchmarkNway
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
      }
  | BenchmarkSingle
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
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

benchmarkNwayOptions :: Parser Command
benchmarkNwayOptions =
  BenchmarkNway <$> configOption <*> baselineModeParser <*> outputFormatParser

benchmarkSingleOptions :: Parser Command
benchmarkSingleOptions =
  BenchmarkSingle <$> configOption <*> baselineModeParser <*> outputFormatParser

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
