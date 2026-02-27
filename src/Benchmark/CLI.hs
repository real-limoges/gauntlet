{-|
Module      : Benchmark.CLI
Description : Command-line argument parsing
Stability   : experimental

Parses CLI arguments for benchmark-multiple, benchmark-single, and verify commands.
-}
module Benchmark.CLI
  ( parseArgs
  , Command (..)
  , BaselineMode (..)
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
  = BenchmarkMultiple
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
      }
  | BenchmarkSingle
      { configPath :: FilePath
      , baselineMode :: BaselineMode
      , outputFormat :: OutputFormat
      }
  | Verify
      { configPath :: FilePath
      , outputFormat :: OutputFormat
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
    ( command "benchmark-multiple" (info benchmarkMultipleOptions (progDesc "Run A/B benchmark"))
        <> command "benchmark-single" (info benchmarkSingleOptions (progDesc "Run single-target benchmark"))
        <> command "verify" (info verifyOptions (progDesc "Verify response equality"))
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
    buildOutputFormat (Just "markdown") path = OutputMarkdown path
    buildOutputFormat _ _ = OutputTerminal

benchmarkMultipleOptions :: Parser Command
benchmarkMultipleOptions =
  BenchmarkMultiple <$> configOption <*> baselineModeParser <*> outputFormatParser

benchmarkSingleOptions :: Parser Command
benchmarkSingleOptions =
  BenchmarkSingle <$> configOption <*> baselineModeParser <*> outputFormatParser

verifyOptions :: Parser Command
verifyOptions = Verify <$> configOption <*> outputFormatParser
