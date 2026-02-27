{-|
Module      : Benchmark.Environment
Description : Git and Docker environment setup
Stability   : experimental

Switches git branches and starts Docker containers for benchmark targets.
-}
module Benchmark.Environment
  ( setupEnvironment
  , waitForHealth
  , trim
  ) where

import Benchmark.Types (PerfTestError (..), Settings (..))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, Response, httpLbs, newManager, parseRequest, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status qualified as Status
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

{-| Switch git branch and optionally start Docker containers for the target service.

Pass 'Nothing' for @composeArgs@ to skip docker-compose (git switch only).
Pass 'Just args' to run @docker-compose@ with @args@ as the complete argument list.
For example, pass @Just ["--profile", "testing", "up", "-d"]@ for the candidate branch and
@Just ["up", "-d", "--build"]@ for the primary.

Uses 'proc' instead of shell strings to prevent shell injection attacks
from malicious branch names containing metacharacters.
-}
setupEnvironment :: Settings -> Text -> Text -> Maybe [String] -> IO (Either PerfTestError ())
setupEnvironment setts branch serviceName composeArgs = do
  let hcPath = fromMaybe "/health" (healthCheckPath setts)
      hcTimeout = fromMaybe 60 (healthCheckTimeout setts)

  gitResult <-
    try (readCreateProcessWithExitCode (proc "git" ["switch", T.unpack branch]) "") ::
      IO (Either IOException (ExitCode, String, String))

  case gitResult of
    Left ex ->
      return $ Left $ EnvironmentSetupError $ "Could not run git: " ++ show ex
    Right (ExitFailure _, _, gitStderr) ->
      return $ Left $ EnvironmentSetupError $ "git switch " ++ T.unpack branch ++ " failed: " ++ trim gitStderr
    Right (ExitSuccess, _, _) ->
      case composeArgs of
        Nothing -> return $ Right ()
        Just args -> do
          dockerResult <-
            try (readCreateProcessWithExitCode (proc "docker-compose" args) "") ::
              IO (Either IOException (ExitCode, String, String))

          case dockerResult of
            Left ex ->
              return $
                Left $
                  EnvironmentSetupError $
                    "Could not run docker-compose: " ++ show ex ++ "\nIs docker-compose installed and in your PATH?"
            Right (ExitFailure _, _, dockerStderr) ->
              return $ Left $ EnvironmentSetupError $ "docker-compose up failed: " ++ trim dockerStderr
            Right (ExitSuccess, _, _) -> do
              mgr <- newManager tlsManagerSettings
              waitForHealth mgr (serviceName <> hcPath) hcTimeout

-- | Poll health endpoint until successful or max retries reached.
waitForHealth :: Manager -> Text -> Int -> IO (Either PerfTestError ())
waitForHealth mgr url maxRetries = loop 0
  where
    loop count
      | count >= maxRetries =
          return $ Left (HealthCheckTimeout url maxRetries)
      | otherwise = do
          result <-
            try (parseRequest (T.unpack url) >>= \req -> httpLbs req mgr) ::
              IO (Either SomeException (Response LBS.ByteString))

          case result of
            Right resp
              | Status.statusCode (responseStatus resp) == 200 ->
                  return $ Right ()
            _ -> do
              threadDelay 1_000_000
              loop (count + 1)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
