module Benchmark.Types.Error
  ( PerfTestError (..)
  , formatError
  , exitWithError
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data PerfTestError
  = ConfigParseError String
  | ConfigValidationError String
  | TokenReadError FilePath String
  | HealthCheckTimeout Text Int
  | NoEndpointsError String
  | EnvironmentSetupError String
  deriving stock (Show, Eq)

formatError :: PerfTestError -> String
formatError (ConfigParseError msg) = "Failed to parse config: " ++ msg
formatError (ConfigValidationError msg) = "Invalid config: " ++ msg
formatError (TokenReadError path msg) = "Failed to read token from " ++ path ++ ": " ++ msg
formatError (HealthCheckTimeout url retries) = "Service at " ++ T.unpack url ++ " failed to start after " ++ show retries ++ " retries"
formatError (NoEndpointsError which) = "No " ++ which ++ " endpoints defined"
formatError (EnvironmentSetupError msg) = "Environment setup failed: " ++ msg

exitWithError :: PerfTestError -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ formatError err) >> exitFailure
