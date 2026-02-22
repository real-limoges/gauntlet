module NetworkSpec (networkSpec) where

import Benchmark.HTTP2 (parseHostPort)
import Test.Hspec

networkSpec :: Spec
networkSpec = describe "Benchmark.HTTP2" $ do
    describe "parseHostPort" $ do
        it "parses host from http URL with no explicit port" $ do
            let (host, port) = parseHostPort "http://example.com/path"
            host `shouldBe` "example.com"
            port `shouldBe` 80

        it "parses host from https URL with no explicit port" $ do
            let (host, port) = parseHostPort "https://example.com/path"
            host `shouldBe` "example.com"
            port `shouldBe` 443

        it "parses explicit port from URL" $ do
            let (host, port) = parseHostPort "http://example.com:8080/path"
            host `shouldBe` "example.com"
            port `shouldBe` 8080

        it "parses localhost with explicit port" $ do
            let (host, port) = parseHostPort "http://localhost:9090"
            host `shouldBe` "localhost"
            port `shouldBe` 9090

        it "falls back to default port for non-numeric port string" $ do
            -- Regression test: previously crashed with `read` on non-numeric input
            let (host, port) = parseHostPort "http://example.com:abc/path"
            host `shouldBe` "example.com"
            port `shouldBe` 80

        it "falls back to 443 for non-numeric port on https URL" $ do
            let (host, port) = parseHostPort "https://example.com:abc/path"
            host `shouldBe` "example.com"
            port `shouldBe` 443
