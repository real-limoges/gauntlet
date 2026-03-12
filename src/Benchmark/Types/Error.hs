module Benchmark.Types.Error
  ( PerfTestError (..)
  , formatError
  , exitWithError
  )
where

import Control.Exception (Exception)
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
  | BenchmarkCancelled
  | -- | Request timed out; contains endpoint URL
    NetworkTimeout Text
  | -- | Connection refused; contains endpoint URL
    ConnectionRefused Text
  | -- | TLS failure; contains endpoint URL and reason
    TlsError Text Text
  | -- | Non-success HTTP response; contains endpoint URL and status code
    HttpError Text Int
  | -- | Catch-all for unrecognised network exceptions
    UnknownNetworkError Text
  deriving stock (Show, Eq)

instance Exception PerfTestError

formatError :: PerfTestError -> String
formatError (ConfigParseError msg) = "Failed to parse config: " ++ msg
formatError (ConfigValidationError msg) = "Invalid config: " ++ msg
formatError (TokenReadError path msg) = "Failed to read token from " ++ path ++ ": " ++ msg
formatError (HealthCheckTimeout url retries) = "Service at " ++ T.unpack url ++ " failed to start after " ++ show retries ++ " retries"
formatError (NoEndpointsError which) = "No " ++ which ++ " endpoints defined"
formatError (EnvironmentSetupError msg) = "Environment setup failed: " ++ msg
formatError BenchmarkCancelled = "Benchmark cancelled by user"
formatError (NetworkTimeout url) = "Request to " ++ T.unpack url ++ " timed out"
formatError (ConnectionRefused url) = "Connection refused: " ++ T.unpack url
formatError (TlsError url reason) = "TLS error connecting to " ++ T.unpack url ++ ": " ++ T.unpack reason
formatError (HttpError url code) = "HTTP " ++ show code ++ " from " ++ T.unpack url
formatError (UnknownNetworkError msg) = "Network error: " ++ T.unpack msg

exitWithError :: PerfTestError -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ formatError err) >> exitFailure
