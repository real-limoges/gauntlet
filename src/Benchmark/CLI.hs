{- |
Module      : Benchmark.CLI
Description : Command-line argument parsing
Stability   : experimental

Parses CLI arguments for benchmark-multiple, benchmark-single, and verify commands.
-}
module Benchmark.CLI (
    parseArgs,
    Command (..),
    BaselineMode (..),
)
where

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
        }
    | BenchmarkSingle
        { configPath :: FilePath
        , baselineMode :: BaselineMode
        }
    | Verify {configPath :: FilePath}
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

benchmarkMultipleOptions :: Parser Command
benchmarkMultipleOptions =
    BenchmarkMultiple <$> configOption <*> baselineModeParser

benchmarkSingleOptions :: Parser Command
benchmarkSingleOptions =
    BenchmarkSingle <$> configOption <*> baselineModeParser

verifyOptions :: Parser Command
verifyOptions = Verify <$> configOption
