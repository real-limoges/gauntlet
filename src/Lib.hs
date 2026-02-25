{- HLINT ignore "Use exitSuccess" -}

{-|
Module      : Lib
Description : Main entry point
Stability   : experimental

CLI dispatch for benchmark and verification commands.
-}
module Lib (run) where

import System.Exit (ExitCode (..), exitWith)

import Benchmark.CLI (Command (..), parseArgs)
import Benchmark.Config (loadConfig, validateConfig)
import Benchmark.Types (PerfTestError (..), RunResult (..), TestConfig, exitWithError)
import Runner (runMultiple, runSingle)
import VerifyRunner (runVerify)

run :: IO ()
run = do
  cmd <- parseArgs
  config <- loadAndValidateConfig (configPath cmd)

  result <- case cmd of
    BenchmarkMultiple _ baseline fmt -> runMultiple baseline fmt config
    BenchmarkSingle _ baseline fmt -> runSingle baseline fmt config
    Verify _ -> runVerify config >> return RunSuccess

  exitWithResult result

{-| Exit with appropriate code based on run result.
0 = success, 1 = regression detected, 2 = error
-}
exitWithResult :: RunResult -> IO ()
exitWithResult RunSuccess = exitWith ExitSuccess
exitWithResult (RunRegression _) = exitWith (ExitFailure 1)
exitWithResult (RunError _) = exitWith (ExitFailure 2)

loadAndValidateConfig :: FilePath -> IO TestConfig
loadAndValidateConfig path = do
  res <- loadConfig path
  case res of
    Left err -> exitWithError $ ConfigParseError err
    Right cfg -> either exitWithError return (validateConfig cfg)
