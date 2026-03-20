-- | Lifecycle hooks: git branch switching, shell hook execution, health-check polling.
module Benchmark.Execution.Environment
  ( runLifecycleSetup
  , runLifecycleTeardown
  , runHook
  , switchBranch
  , waitForHealth
  ) where

import Benchmark.Types.Config
  ( HealthCheckConfig (..)
  , HookCommand (..)
  , LifecycleHooks (..)
  , NamedTarget (..)
  )
import Benchmark.Types.Error (PerfTestError (..))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, responseStatus)
import Network.HTTP.Types.Status qualified as Status
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, proc, shell)
import System.Timeout (timeout)

{-| Run setup phase for a target: optionally switch git branch, run setup hook,
then wait for health check.
-}
runLifecycleSetup :: Manager -> NamedTarget -> IO (Either PerfTestError ())
runLifecycleSetup mgr target = do
  branchResult <- case targetBranch target of
    Nothing -> return (Right ())
    Just b | T.null b -> return (Right ())
    Just b -> switchBranch b
  case branchResult of
    Left e -> return (Left e)
    Right () -> case targetLifecycle target of
      Nothing -> return (Right ())
      Just hooks -> do
        setupResult <- case hookSetup hooks of
          Nothing -> return (Right ())
          Just cmd -> runHook cmd HookSetupError
        case setupResult of
          Left e -> return (Left e)
          Right () -> case hookHealthCheck hooks of
            Nothing -> return (Right ())
            Just hc -> waitForHealth mgr hc

-- | Run the teardown hook for a target, if one is configured.
runLifecycleTeardown :: NamedTarget -> IO (Either PerfTestError ())
runLifecycleTeardown target = case targetLifecycle target of
  Nothing -> return (Right ())
  Just hooks -> case hookTeardown hooks of
    Nothing -> return (Right ())
    Just cmd -> runHook cmd HookTeardownError

{-| Run a shell command as a lifecycle hook.

The command is executed via @\/bin\/sh -c@. If 'hookTimeoutSecs' is set
(default: 300 s) and the command does not finish in time, returns
'HookTimeoutError'. Non-zero exit codes return the provided error constructor
with the captured stderr.
-}
runHook :: HookCommand -> (Text -> PerfTestError) -> IO (Either PerfTestError ())
runHook HookCommand{..} mkError = do
  let timeoutUs = fromMaybe 300 hookTimeoutSecs * 1_000_000
      proc_ = (shell (T.unpack hookCmd)) {cwd = hookWorkingDir}
  mResult <-
    timeout timeoutUs $
      try (readCreateProcessWithExitCode proc_ "") ::
        IO (Maybe (Either IOException (ExitCode, String, String)))
  case mResult of
    Nothing ->
      return $ Left $ HookTimeoutError hookCmd (fromMaybe 300 hookTimeoutSecs)
    Just (Left ex) ->
      return $ Left $ mkError $ "Could not run command: " <> T.pack (show ex)
    Just (Right (ExitFailure _, _, stderr)) ->
      return $ Left $ mkError $ hookCmd <> " failed: " <> T.strip (T.pack stderr)
    Just (Right (ExitSuccess, _, _)) ->
      return (Right ())

-- | Switch to a git branch using @git switch@.
switchBranch :: Text -> IO (Either PerfTestError ())
switchBranch branch = do
  r <-
    try (readCreateProcessWithExitCode (proc "git" ["switch", T.unpack branch]) "") ::
      IO (Either IOException (ExitCode, String, String))
  case r of
    Left ex ->
      return $ Left $ GitSwitchError $ "Could not run git: " <> T.pack (show ex)
    Right (ExitFailure _, _, stderr) ->
      return $ Left $ GitSwitchError $ "git switch " <> branch <> " failed: " <> T.strip (T.pack stderr)
    Right (ExitSuccess, _, _) ->
      return (Right ())

-- | Poll the health URL until it returns 200 or the timeout budget is exhausted.
waitForHealth :: Manager -> HealthCheckConfig -> IO (Either PerfTestError ())
waitForHealth mgr HealthCheckConfig{..} = loop 0
  where
    maxRetries = fromMaybe 60 hcTimeoutSecs
    intervalUs = fromMaybe 1000 hcIntervalMs * 1000
    loop count
      | count >= maxRetries =
          return $ Left (HealthCheckTimeout hcUrl maxRetries)
      | otherwise = do
          result <-
            try (parseRequest (T.unpack hcUrl) >>= \req -> httpLbs req mgr) ::
              IO (Either SomeException (Response LBS.ByteString))
          case result of
            Right resp
              | Status.statusCode (responseStatus resp) == 200 ->
                  return $ Right ()
            _ -> do
              threadDelay intervalUs
              loop (count + 1)
