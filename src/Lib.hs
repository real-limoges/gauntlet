{- HLINT ignore "Use exitSuccess" -}

{-|
Module      : Lib
Description : Main entry point
Stability   : experimental

CLI dispatch for benchmark commands.
-}
module Lib (run) where

import System.Exit (ExitCode (..), exitWith)

import Benchmark.CLI (Command (..), parseArgs)
import Benchmark.Config (loadConfig, loadNwayConfig, validateConfig, validateNwayConfig)
import Benchmark.Types
  ( NwayConfig
  , PerfTestError (..)
  , RunResult (..)
  , TestConfig
  , exitWithError
  )
import Runner (runSingle)
import Runner.Nway (runNway)

run :: IO ()
run = do
  cmd <- parseArgs
  result <- case cmd of
    BenchmarkNway path baseline fmt -> do
      cfg <- loadAndValidateNwayConfig path
      runNway baseline fmt cfg
    BenchmarkSingle path baseline fmt -> do
      cfg <- loadAndValidateConfig path
      runSingle baseline fmt cfg
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

loadAndValidate :: (FilePath -> IO (Either String a)) -> (a -> Either PerfTestError a) -> FilePath -> IO a
loadAndValidate load validate path = do
  res <- load path
  case res of
    Left err -> exitWithError $ ConfigParseError err
    Right cfg -> either exitWithError return (validate cfg)
