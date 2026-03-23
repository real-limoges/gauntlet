{- HLINT ignore "Use exitSuccess" -}

-- | Main entry point: CLI dispatch and benchmark orchestration.
module Lib (run) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, httpNoBody, newManager, parseRequest, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import Benchmark.Compare (runCompare)
import Benchmark.Config.CLI (Command (..), parseArgs)
import Benchmark.Config.Loader (loadBenchmarkConfig, validateBenchmarkConfig)
import Benchmark.Reporter (Reporter, ciReporter, combineReporters, markdownReporter, terminalReporter)
import Benchmark.Reporter.HTML (htmlReporter)
import Benchmark.Reporter.JUnit (junitReporter)
import Benchmark.Reporter.Prometheus (prometheusReporter)
import Benchmark.Types
  ( BenchmarkConfig (..)
  , ChartsSettings
  , HealthCheckConfig (..)
  , LifecycleHooks (..)
  , LogLevel (..)
  , NamedTarget (..)
  , OutputFormat (..)
  , PerfTestError (..)
  , RunResult (..)
  , Settings (..)
  , exitWithError
  )
import Log (makeLogger)
import Runner.Benchmark (runBenchmark)

-- | Parse CLI arguments and dispatch to the appropriate benchmark command.
run :: IO ()
run = do
  cmd <- parseArgs
  ci <- ciReporter
  promReporter <- mkPrometheusReporter cmd
  let reporter =
        combineReporters $
          catMaybes
            [ Just terminalReporter
            , markdownReporter <$> getMarkdownPath cmd
            , junitReporter <$> getJUnitPath cmd
            , htmlReporter <$> getHTMLPath cmd
            , promReporter
            , Just ci
            ]
  let charts = getChartsConfig cmd
  result <- case cmd of
    Benchmark {configPath = path, baselineMode = baseline} -> do
      cfg <- loadConfig path
      runBenchmark reporter baseline charts cfg
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

loadConfig :: FilePath -> IO BenchmarkConfig
loadConfig path = do
  res <- loadBenchmarkConfig path
  case res of
    Left err -> exitWithError $ ConfigParseError (T.pack err)
    Right cfg -> either exitWithError return (validateBenchmarkConfig cfg)

getMarkdownPath :: Command -> Maybe FilePath
getMarkdownPath Benchmark {outputFormat = fmt} = case fmt of OutputMarkdown p -> Just p; _ -> Nothing
getMarkdownPath _ = Nothing

getChartsConfig :: Command -> Maybe ChartsSettings
getChartsConfig Benchmark {chartsConfig = c} = c
getChartsConfig _ = Nothing

getJUnitPath :: Command -> Maybe FilePath
getJUnitPath Benchmark {junitPath = j} = j
getJUnitPath _ = Nothing

getHTMLPath :: Command -> Maybe FilePath
getHTMLPath Benchmark {htmlPath = h} = h
getHTMLPath _ = Nothing

mkPrometheusReporter :: Command -> IO (Maybe Reporter)
mkPrometheusReporter Benchmark {prometheusConfig = Just (url, job)} = do
  let logger = makeLogger Info
  mgr <- newManager tlsManagerSettings
  pure $ Just $ prometheusReporter logger mgr (T.pack url) (T.pack job)
mkPrometheusReporter _ = pure Nothing

-- | Validate a config file without running any benchmarks.
runValidate :: FilePath -> Bool -> IO RunResult
runValidate cfgPath doCheck = do
  res <- loadBenchmarkConfig cfgPath
  case res of
    Left err -> do
      hPutStrLn stderr $ "Config error: " <> err
      pure (RunError (ConfigParseError (T.pack err)))
    Right cfg -> case validateBenchmarkConfig cfg of
      Left err -> do
        hPutStrLn stderr $ "Validation error: " <> show err
        pure (RunError err)
      Right validCfg -> do
        let setts = benchSettings validCfg
            targets = benchTargets validCfg
            payloads = benchPayloads validCfg
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
            results <- mapM (checkEndpoint mgr) targets
            if and results
              then pure RunSuccess
              else pure (RunError (UnknownNetworkError "one or more endpoints unreachable"))
          else pure RunSuccess

checkEndpoint :: Manager -> NamedTarget -> IO Bool
checkEndpoint mgr target =
  catch go (\(_ :: SomeException) -> putStrLn unreachableMsg >> pure False)
  where
    url = T.unpack $ case targetLifecycle target >>= hookHealthCheck of
      Just hc -> hcUrl hc
      Nothing -> targetUrl target <> "/health"
    unreachableMsg = "  " <> T.unpack (targetName target) <> " " <> url <> " => unreachable"
    go = do
      req <- parseRequest url
      t0 <- getCurrentTime
      resp <- httpNoBody req mgr
      t1 <- getCurrentTime
      let ms = round (diffUTCTime t1 t0 * 1000) :: Int
          code = statusCode (responseStatus resp)
      putStrLn $
        "  " <> T.unpack (targetName target) <> " " <> url <> " => HTTP " <> show code <> " (" <> show ms <> "ms)"
      pure (code >= 200 && code < 400)
