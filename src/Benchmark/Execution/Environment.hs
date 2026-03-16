-- | Git branch switching, docker-compose orchestration, and health-check polling.
module Benchmark.Execution.Environment
  ( Branch (..)
  , ServiceUrl (..)
  , setupEnvironment
  , waitForHealth
  ) where

import Benchmark.Types (PerfTestError (..), Settings (..))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, responseStatus)
import Network.HTTP.Types.Status qualified as Status
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

-- | A git branch name.
newtype Branch = Branch Text
  deriving newtype (Show, Eq, IsString)

-- | A service base URL for health checking.
newtype ServiceUrl = ServiceUrl Text
  deriving newtype (Show, Eq, IsString)

{-| Switch git branch and optionally start Docker containers for the target service.

Pass 'Nothing' for @composeArgs@ to skip docker-compose (git switch only).
Pass 'Just args' to run @docker-compose@ with @args@ as the complete argument list.
For example, pass @Just ["--profile", "testing", "up", "-d"]@ for the candidate branch and
@Just ["up", "-d", "--build"]@ for the primary.

Uses 'proc' instead of shell strings to prevent shell injection attacks
from malicious branch names containing metacharacters.
-}
setupEnvironment ::
  Manager -> Settings -> Branch -> ServiceUrl -> Maybe [String] -> IO (Either PerfTestError ())
setupEnvironment mgr setts (Branch branch) (ServiceUrl serviceName) composeArgs = do
  let hcPath = fromMaybe "/health" (healthCheckPath setts)
      hcTimeout = fromMaybe 60 (healthCheckTimeout setts)
  switchResult <- switchBranch branch
  case switchResult of
    Left e -> return (Left e)
    Right () -> case composeArgs of
      Nothing -> return (Right ())
      Just args -> do
        dockerResult <- startDocker args
        case dockerResult of
          Left e -> return (Left e)
          Right () -> waitForHealth mgr (serviceName <> hcPath) hcTimeout
  where
    switchBranch b = do
      r <-
        try (readCreateProcessWithExitCode (proc "git" ["switch", T.unpack b]) "") ::
          IO (Either IOException (ExitCode, String, String))
      case r of
        Left ex -> return $ Left $ EnvironmentSetupError $ "Could not run git: " <> T.pack (show ex)
        Right (ExitFailure _, _, stderr) ->
          return $ Left $ EnvironmentSetupError $ "git switch " <> b <> " failed: " <> T.strip (T.pack stderr)
        Right (ExitSuccess, _, _) -> return (Right ())

    startDocker args = do
      r <-
        try (readCreateProcessWithExitCode (proc "docker-compose" args) "") ::
          IO (Either IOException (ExitCode, String, String))
      case r of
        Left ex ->
          return $
            Left $
              EnvironmentSetupError $
                "Could not run docker-compose: " <> T.pack (show ex) <> "\nIs docker-compose installed and in your PATH?"
        Right (ExitFailure _, _, stderr) ->
          return $ Left $ EnvironmentSetupError $ "docker-compose up failed: " <> T.strip (T.pack stderr)
        Right (ExitSuccess, _, _) -> return (Right ())

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
