module ConfigSpec (configSpec) where

import Benchmark.Config
import Benchmark.Types
import Test.Hspec
import TestHelpers

configSpec :: Spec
configSpec = describe "Benchmark.Config" $ do
    describe "validateConfig" $ do
        it "accepts valid configuration" $ do
            let cfg = makeValidConfig
            validateConfig cfg `shouldBe` Right cfg

        it "rejects empty payloads" $ do
            let cfg = makeValidConfig{payloads = []}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "No payloads defined in config"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects zero iterations" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){iterations = 0}}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "iterations must be greater than 0"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects negative iterations" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){iterations = -5}}
            case validateConfig cfg of
                Left (ConfigValidationError _) -> pure ()
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects zero concurrency" $ do
            let cfg = makeValidConfig{settings = (settings makeValidConfig){concurrency = 0}}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldBe` "concurrency must be greater than 0"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "rejects invalid HTTP methods" $ do
            let badPayload = PayloadSpec "test" "INVALID" "/path" Nothing
            let cfg = makeValidConfig{payloads = [badPayload]}
            case validateConfig cfg of
                Left (ConfigValidationError msg) ->
                    msg `shouldContain` "Invalid HTTP method"
                _ -> expectationFailure "Expected ConfigValidationError"

        it "accepts all valid HTTP methods" $ do
            let methods = ["GET", "POST", "PUT", "DELETE", "PATCH"]
            let mkPayload m = PayloadSpec "test" m "/path" Nothing
            let cfg = makeValidConfig{payloads = map mkPayload methods}
            validateConfig cfg `shouldBe` Right cfg

    describe "buildEndpoints" $ do
        it "builds endpoints for primary target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints cfg False
            case endpoints of
                [ep] -> url ep `shouldBe` "http://primary.test/api/test"
                _ -> expectationFailure "Expected exactly one endpoint"

        it "builds endpoints for candidate target" $ do
            let cfg = makeValidConfig
            let endpoints = buildEndpoints cfg True
            case endpoints of
                [ep] -> url ep `shouldBe` "http://candidate.test/api/test"
                _ -> expectationFailure "Expected exactly one endpoint"

        it "preserves method and body from payload spec" $ do
            let cfg = makeValidConfig
            case buildEndpoints cfg False of
                [ep] -> do
                    method ep `shouldBe` "POST"
                    body ep `shouldBe` Nothing
                _ -> expectationFailure "Expected exactly one endpoint"

        it "sets content-type header" $ do
            let cfg = makeValidConfig
            case buildEndpoints cfg False of
                [ep] -> headers ep `shouldContain` [("Content-Type", "application/json")]
                _ -> expectationFailure "Expected exactly one endpoint"
