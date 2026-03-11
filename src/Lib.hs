{- HLINT ignore "Use exitSuccess" -}

module Lib (run) where

import System.Exit (ExitCode (..), exitWith)

import Benchmark.Config.CLI (Command (..), parseArgs)
import Benchmark.Config.Loader (loadConfig, loadNwayConfig, validateConfig, validateNwayConfig)
import Benchmark.Reporter (combineReporters)
import Benchmark.Reporter.CI (ciReporter)
import Benchmark.Reporter.Markdown (markdownReporter)
import Benchmark.Reporter.Terminal (terminalReporter)
import Benchmark.Types
  ( NwayConfig
  , OutputFormat (..)
  , PerfTestError (..)
  , RunResult (..)
  , TestConfig
  , exitWithError
  )
import Data.Maybe (catMaybes)
import Runner (runSingle)
import Runner.Nway (runNway)

run :: IO ()
run = do
  cmd <- parseArgs
  ci <- ciReporter
  let reporter =
        combineReporters $
          catMaybes
            [ Just terminalReporter
            , markdownReporter <$> getMarkdownPath cmd
            , Just ci
            ]
  result <- case cmd of
    BenchmarkNway path baseline _ -> do
      cfg <- loadAndValidateNwayConfig path
      runNway reporter baseline cfg
    BenchmarkSingle path baseline _ -> do
      cfg <- loadAndValidateConfig path
      runSingle reporter baseline cfg
  exitWithResult result

{-| Exit with appropriate code based on run result.
0 = success, 1 = regression detected, 2 = error
-}
exitWithResult :: RunResult -> IO ()
exitWithResult RunSuccess = exitWith ExitSuccess
exitWithResult (RunRegression _) = exitWith (ExitFailure 1)
exitWithResult (RunError _) = exitWith (ExitFailure 2)

loadAndValidateConfig :: FilePath -> IO TestConfig
loadAndValidateConfig = loadAndValidate loadConfig validateConfig

loadAndValidateNwayConfig :: FilePath -> IO NwayConfig
loadAndValidateNwayConfig = loadAndValidate loadNwayConfig validateNwayConfig

getMarkdownPath :: Command -> Maybe FilePath
getMarkdownPath cmd = case outputFormat cmd of
  OutputMarkdown path -> Just path
  OutputTerminal -> Nothing

loadAndValidate :: (FilePath -> IO (Either String a)) -> (a -> Either PerfTestError a) -> FilePath -> IO a
loadAndValidate load validate path = do
  res <- load path
  case res of
    Left err -> exitWithError $ ConfigParseError err
    Right cfg -> either exitWithError return (validate cfg)
