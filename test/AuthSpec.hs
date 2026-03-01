module AuthSpec (authSpec) where

import Benchmark.Network.Auth (addAuth, readToken)
import Benchmark.Types (Endpoint (..), PerfTestError (..))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

authSpec :: TestTree
authSpec =
  testGroup
    "Benchmark.Network.Auth"
    [ testGroup
        "addAuth"
        [ testCase "injects Authorization header when token is non-empty" $ do
            let ep = baseEndpoint
            let ep' = addAuth "my-secret-token" ep
            lookup "Authorization" (headers ep') `shouldBe` Just "Bearer my-secret-token"
        , testCase "is a no-op when token is empty" $ do
            let ep = baseEndpoint
            let ep' = addAuth "" ep
            headers ep' `shouldBe` headers ep
        , testCase "prepends auth header, preserving existing headers" $ do
            let ep = baseEndpoint {headers = [("X-Custom", "value")]}
            let ep' = addAuth "tok" ep
            lookup "X-Custom" (headers ep') `shouldBe` Just "value"
            lookup "Authorization" (headers ep') `shouldBe` Just "Bearer tok"
        , testCase "existing headers survive when token is empty" $ do
            let ep = baseEndpoint {headers = [("X-Keep", "yes")]}
            let ep' = addAuth "" ep
            headers ep' `shouldBe` headers ep
        ]
    , testGroup
        "readToken"
        [ testCase "reads token from file and strips trailing whitespace" $
            withSystemTempFile "token.txt" $ \path h -> do
              hPutStr h "my-token\n"
              hClose h
              result <- readToken path
              result `shouldBe` Right "my-token"
        , testCase "strips leading and trailing whitespace" $
            withSystemTempFile "token.txt" $ \path h -> do
              hPutStr h "  spaced-token  \n"
              hClose h
              result <- readToken path
              result `shouldBe` Right "spaced-token"
        , testCase "returns Left for a missing file" $ do
            result <- readToken "/tmp/gauntlet-test-nonexistent-file-xyz"
            case result of
              Left (TokenReadError _ _) -> pure ()
              Left other -> assertFailure $ "Expected TokenReadError, got: " ++ show other
              Right _ -> assertFailure "Expected Left for missing file"
        , testCase "reads an empty file as empty text" $
            withSystemTempFile "token.txt" $ \path h -> do
              hClose h
              result <- readToken path
              result `shouldBe` Right ""
        ]
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

baseEndpoint :: Endpoint
baseEndpoint = Endpoint "GET" "http://example.com" Nothing [] Nothing
