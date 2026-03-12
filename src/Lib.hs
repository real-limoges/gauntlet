{- HLINT ignore "Use exitSuccess" -}

-- | Main entry point: CLI dispatch and benchmark orchestration.
module Lib (run) where

import Control.Monad (forM_)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, httpNoBody, newManager, parseRequest, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Benchmark.Compare (runCompare)
import Benchmark.Config.CLI (Command (..), parseArgs)
import Benchmark.Config.Loader (loadConfig, loadNwayConfig, validateConfig, validateNwayConfig)
import Benchmark.Reporter (combineReporters)
import Benchmark.Reporter.CI (ciReporter)
import Benchmark.Reporter.Markdown (markdownReporter)
import Benchmark.Reporter.Terminal (terminalReporter)
import Benchmark.Types
  ( ChartsSettings
  , NwayConfig (..)
  , OutputFormat (..)
  , PerfTestError (..)
  , RunResult (..)
  , TestConfig
  , exitWithError
  )
import Benchmark.Types.Config (NamedTarget (..), Settings (..))
import Runner (runSingle)
import Runner.Nway (runNway)

-- | Parse CLI arguments and dispatch to the appropriate benchmark command.
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
  let charts = getChartsConfig cmd
  result <- case cmd of
    BenchmarkNway path baseline _ _ -> do
      cfg <- loadAndValidateNwayConfig path
      runNway reporter baseline charts cfg
    BenchmarkSingle path baseline _ _ -> do
      cfg <- loadAndValidateConfig path
      runSingle reporter baseline charts cfg
    Compare fileA fileB ->
      runCompare reporter fileA fileB
    Validate cfgPath doCheck ->
      runValidate cfgPath doCheck
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
getMarkdownPath (BenchmarkNway _ _ fmt _) = case fmt of OutputMarkdown p -> Just p; _ -> Nothing
getMarkdownPath (BenchmarkSingle _ _ fmt _) = case fmt of OutputMarkdown p -> Just p; _ -> Nothing
getMarkdownPath _ = Nothing

getChartsConfig :: Command -> Maybe ChartsSettings
getChartsConfig (BenchmarkNway _ _ _ c) = c
getChartsConfig (BenchmarkSingle _ _ _ c) = c
getChartsConfig _ = Nothing

loadAndValidate :: (FilePath -> IO (Either String a)) -> (a -> Either PerfTestError a) -> FilePath -> IO a
loadAndValidate load validate path = do
  res <- load path
  case res of
    Left err -> exitWithError $ ConfigParseError (T.pack err)
    Right cfg -> either exitWithError return (validate cfg)

-- | Validate a config file without running any benchmarks.
runValidate :: FilePath -> Bool -> IO RunResult
runValidate cfgPath doCheck = do
  res <- loadNwayConfig cfgPath
  case res of
    Left err -> do
      hPutStrLn stderr $ "Config error: " <> err
      pure (RunError (ConfigParseError (T.pack err)))
    Right cfg -> case validateNwayConfig cfg of
      Left err -> do
        hPutStrLn stderr $ "Validation error: " <> show err
        pure (RunError err)
      Right validCfg -> do
        let setts = nwaySettings validCfg
            targets = nwayTargets validCfg
            payloads = nwayPayloads validCfg
            hcPath = fromMaybe "/health" (healthCheckPath setts)
        putStrLn "Config OK"
        putStrLn $ "Targets (" <> show (length targets) <> "):"
        forM_ targets $ \t ->
          putStrLn $ "  " <> T.unpack (targetName t) <> " => " <> T.unpack (targetUrl t)
        putStrLn $ "Payloads:    " <> show (length payloads)
        putStrLn $ "Iterations:  " <> show (iterations setts)
        putStrLn $ "Concurrency: " <> show (concurrency setts)
        putStrLn $ "Load mode:   " <> maybe "unthrottled" show (loadMode setts)
        if doCheck
          then do
            mgr <- newManager tlsManagerSettings
            results <- mapM (checkEndpoint mgr hcPath) targets
            if and results
              then pure RunSuccess
              else pure (RunError (UnknownNetworkError "one or more endpoints unreachable"))
          else pure RunSuccess

checkEndpoint :: Manager -> T.Text -> NamedTarget -> IO Bool
checkEndpoint mgr hcPath target = do
  let url = T.unpack (targetUrl target) <> T.unpack hcPath
  req <- parseRequest url
  t0 <- getCurrentTime
  resp <- httpNoBody req mgr
  t1 <- getCurrentTime
  let ms = round (diffUTCTime t1 t0 * 1000) :: Int
      code = statusCode (responseStatus resp)
  putStrLn $
    "  " <> T.unpack (targetName target) <> " " <> url <> " => HTTP " <> show code <> " (" <> show ms <> "ms)"
  pure (code >= 200 && code < 400)
