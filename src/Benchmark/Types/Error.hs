module Benchmark.Types.Error
  ( PerfTestError (..)
  , formatError
  , exitWithError
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (stderr)

data PerfTestError
  = ConfigParseError Text
  | ConfigValidationError Text
  | TokenReadError Text Text
  | HealthCheckTimeout Text Int
  | NoEndpointsError Text
  | EnvironmentSetupError Text
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

formatError :: PerfTestError -> Text
formatError (ConfigParseError msg) = "Config parse error: " <> msg
formatError (ConfigValidationError msg) = "Config validation error: " <> msg
formatError (TokenReadError path msg) = "Token read error (" <> path <> "): " <> msg
formatError (HealthCheckTimeout url retries) =
  "Health check timeout: " <> url <> " failed after " <> T.pack (show retries) <> " retries"
formatError (NoEndpointsError which) = "No " <> which <> " endpoints defined"
formatError (EnvironmentSetupError msg) = "Environment setup error: " <> msg
formatError BenchmarkCancelled = "Benchmark cancelled by user"
formatError (NetworkTimeout url) = "Network timeout: " <> url
formatError (ConnectionRefused url) = "Connection refused: " <> url
formatError (TlsError url reason) = "TLS error (" <> url <> "): " <> reason
formatError (HttpError url code) = "HTTP " <> T.pack (show code) <> " from " <> url
formatError (UnknownNetworkError msg) = "Network error: " <> msg

exitWithError :: PerfTestError -> IO a
exitWithError err = TIO.hPutStrLn stderr ("Error: " <> formatError err) >> exitFailure
