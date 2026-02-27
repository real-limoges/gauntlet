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
import Benchmark.Config (loadConfig, loadNwayConfig, validateConfig, validateNwayConfig)
import Benchmark.Types
  ( NwayConfig
  , PerfTestError (..)
  , RegressionResult (..)
  , RunResult (..)
  , TestConfig
  , exitWithError
  )
import Runner (runSingle)
import Runner.Nway (runNway)
import VerifyRunner (runVerify)

run :: IO ()
run = do
  cmd <- parseArgs
  result <- case cmd of
    BenchmarkNway path fmt -> do
      cfg <- loadAndValidateNwayConfig path
      runNway fmt cfg
    BenchmarkSingle path baseline fmt -> do
      cfg <- loadAndValidateConfig path
      runSingle baseline fmt cfg
    Verify {configPath = path, outputFormat = fmt} -> do
      cfg <- loadAndValidateConfig path
      passed <- runVerify fmt cfg
      return $ if passed then RunSuccess else RunRegression (RegressionResult "verify" [] False)

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

loadAndValidateNwayConfig :: FilePath -> IO NwayConfig
loadAndValidateNwayConfig path = do
  res <- loadNwayConfig path
  case res of
    Left err -> exitWithError $ ConfigParseError err
    Right cfg -> either exitWithError return (validateNwayConfig cfg)
