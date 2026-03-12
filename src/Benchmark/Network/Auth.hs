module Benchmark.Network.Auth (readToken, addAuth) where

import Benchmark.Types (Endpoint (..), PerfTestError (..))
import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Read a bearer token from a file, stripping whitespace.
readToken :: FilePath -> IO (Either PerfTestError Text)
readToken path = do
  result <- try (TIO.readFile path) :: IO (Either SomeException Text)
  return $ case result of
    Left err -> Left (TokenReadError (T.pack path) (T.pack (show err)))
    Right raw -> Right (T.strip raw)

{-| Prepend an @Authorization: Bearer <token>@ header to an endpoint.
No-ops when the token is empty (no auth file configured).
-}
addAuth :: Text -> Endpoint -> Endpoint
addAuth token ep
  | T.null token = ep
  | otherwise =
      let authHeader = ("Authorization", "Bearer " <> token)
       in ep {headers = authHeader : headers ep}
