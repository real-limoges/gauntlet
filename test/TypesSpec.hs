module TypesSpec (typesSpec) where

import Benchmark.Environment (trim)
import Benchmark.Types
import Test.Hspec

typesSpec :: Spec
typesSpec = do
  describe "Benchmark.Types" $ do
    describe "nsToMs" $ do
      it "converts 1_000_000 ns to 1.0 ms" $ do
        nsToMs (Nanoseconds 1_000_000) `shouldBe` Milliseconds 1.0

      it "converts 0 ns to 0.0 ms" $ do
        nsToMs (Nanoseconds 0) `shouldBe` Milliseconds 0.0

      it "converts sub-millisecond values correctly" $ do
        nsToMs (Nanoseconds 500_000) `shouldBe` Milliseconds 0.5

      it "converts large values correctly" $ do
        nsToMs (Nanoseconds 1_500_000_000) `shouldBe` Milliseconds 1500.0

    describe "formatError" $ do
      it "formats ConfigParseError" $ do
        formatError (ConfigParseError "bad json") `shouldBe` "Failed to parse config: bad json"

      it "formats ConfigValidationError" $ do
        formatError (ConfigValidationError "missing field") `shouldBe` "Invalid config: missing field"

      it "formats TokenReadError with path and message" $ do
        formatError (TokenReadError "/secrets.txt" "permission denied")
          `shouldBe` "Failed to read token from /secrets.txt: permission denied"

      it "formats HealthCheckTimeout with URL and retry count" $ do
        formatError (HealthCheckTimeout "http://svc:8080/health" 60)
          `shouldBe` "Service at http://svc:8080/health failed to start after 60 retries"

      it "formats NoEndpointsError" $ do
        formatError (NoEndpointsError "primary") `shouldBe` "No primary endpoints defined"

      it "formats EnvironmentSetupError" $ do
        formatError (EnvironmentSetupError "git checkout failed")
          `shouldBe` "Environment setup failed: git checkout failed"

  describe "Benchmark.Environment.trim" $ do
    it "strips leading and trailing spaces" $ do
      trim "  hello  " `shouldBe` "hello"

    it "strips tabs and newlines" $ do
      trim "\n\tfoo\r\n" `shouldBe` "foo"

    it "returns empty for all-whitespace input" $ do
      trim "  \t\n  " `shouldBe` ""

    it "returns empty for empty input" $ do
      trim "" `shouldBe` ""

    it "leaves non-whitespace strings unchanged" $ do
      trim "no spaces" `shouldBe` "no spaces"

    it "preserves internal whitespace" $ do
      trim "  hello world  " `shouldBe` "hello world"
