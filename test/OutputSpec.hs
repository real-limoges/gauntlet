module OutputSpec (outputSpec) where

import Benchmark.Output
import Benchmark.Types (Endpoint (..), Nanoseconds (..), TestingResponse (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (toLazyText)
import Test.Hspec

outputSpec :: Spec
outputSpec = describe "Benchmark.Output" $ do
  describe "resultsDir" $ do
    it "is the string \"results\"" $ do
      resultsDir `shouldBe` "results"

  describe "initOutputFiles" $ do
    it "creates a CSV file with the correct header" $ do
      (csvFile, timestamp) <- initOutputFiles
      csvFile `shouldSatisfy` (/= "")
      timestamp `shouldSatisfy` (/= "")
      header <- TIO.readFile csvFile
      header `shouldSatisfy` T.isInfixOf "payload_id"
      header `shouldSatisfy` T.isInfixOf "url"
      header `shouldSatisfy` T.isInfixOf "status_code"
      header `shouldSatisfy` T.isInfixOf "latency_ms"

  describe "writeLatencies" $ do
    it "writes one row per response" $ do
      (csvFile, _) <- initOutputFiles
      let ep = testEndpoint "http://example.com/api"
      let responses = [makeResp 200 1_000_000, makeResp 200 2_000_000]
      writeLatencies csvFile [(1, ep, responses)]
      contents <- TIO.readFile csvFile
      -- header + 2 data rows
      let rows = filter (not . T.null) (T.lines contents)
      length rows `shouldBe` 3

    it "CSV rows contain the endpoint URL" $ do
      (csvFile, _) <- initOutputFiles
      let ep = testEndpoint "http://example.com/check"
      writeLatencies csvFile [(1, ep, [makeResp 200 500_000])]
      contents <- TIO.readFile csvFile
      contents `shouldSatisfy` T.isInfixOf "http://example.com/check"

    it "CSV rows contain the status code" $ do
      (csvFile, _) <- initOutputFiles
      let ep = testEndpoint "http://x.com"
      writeLatencies csvFile [(1, ep, [makeResp 404 100_000])]
      contents <- TIO.readFile csvFile
      contents `shouldSatisfy` T.isInfixOf "404"

    it "handles multiple endpoints without crashing" $ do
      (csvFile, _) <- initOutputFiles
      let ep1 = testEndpoint "http://a.com"
          ep2 = testEndpoint "http://b.com"
      writeLatencies
        csvFile
        [ (1, ep1, [makeResp 200 1_000_000])
        , (2, ep2, [makeResp 200 2_000_000])
        ]
      contents <- TIO.readFile csvFile
      contents `shouldSatisfy` T.isInfixOf "http://a.com"
      contents `shouldSatisfy` T.isInfixOf "http://b.com"

    it "handles error response (status 0) without crashing" $ do
      (csvFile, _) <- initOutputFiles
      let ep = testEndpoint "http://x.com"
      writeLatencies csvFile [(1, ep, [makeResp 0 0])]
      contents <- TIO.readFile csvFile
      -- Just check it doesn't crash and still has header
      contents `shouldSatisfy` T.isInfixOf "payload_id"

    it "writes the correct payload_id in each row" $ do
      (csvFile, _) <- initOutputFiles
      let ep = testEndpoint "http://x.com"
      writeLatencies csvFile [(42, ep, [makeResp 200 100_000])]
      contents <- TIO.readFile csvFile
      contents `shouldSatisfy` T.isInfixOf "42"

  describe "formatRow (pure Builder)" $ do
    it "produces comma-separated fields" $ do
      let ep = testEndpoint "http://example.com/api"
          r = makeResp 200 1_000_000
          row = TL.toStrict $ toLazyText $ formatRow 1 ep r
          fields = T.splitOn "," row
      length fields `shouldBe` 4

    it "includes the payload index" $ do
      let row = TL.toStrict $ toLazyText $ formatRow 42 (testEndpoint "http://x.com") (makeResp 200 100)
      T.isPrefixOf "42," row `shouldBe` True

    it "includes the endpoint URL" $ do
      let row = TL.toStrict $ toLazyText $ formatRow 1 (testEndpoint "http://my-service.com/path") (makeResp 200 100)
      row `shouldSatisfy` T.isInfixOf "http://my-service.com/path"

    it "includes the status code" $ do
      let row = TL.toStrict $ toLazyText $ formatRow 1 (testEndpoint "http://x.com") (makeResp 404 100)
      row `shouldSatisfy` T.isInfixOf "404"

    it "includes the raw nanosecond duration" $ do
      let row = TL.toStrict $ toLazyText $ formatRow 1 (testEndpoint "http://x.com") (makeResp 200 1_500_000)
      row `shouldSatisfy` T.isInfixOf "1500000"

    it "ends with a newline" $ do
      let row = TL.toStrict $ toLazyText $ formatRow 1 (testEndpoint "http://x.com") (makeResp 200 100)
      T.isSuffixOf "\n" row `shouldBe` True

  describe "formatResultBuilder (pure Builder)" $ do
    it "produces one row per response" $ do
      let ep = testEndpoint "http://x.com"
          responses = [makeResp 200 1_000, makeResp 200 2_000, makeResp 200 3_000]
          output = TL.toStrict $ toLazyText $ formatResultBuilder (1, ep, responses)
          rows = filter (not . T.null) (T.lines output)
      length rows `shouldBe` 3

    it "produces empty output for empty responses" $ do
      let output = TL.toStrict $ toLazyText $ formatResultBuilder (1, testEndpoint "http://x.com", [])
      output `shouldBe` ""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

testEndpoint :: T.Text -> Endpoint
testEndpoint u = Endpoint "GET" u Nothing [] Nothing

makeResp :: Int -> Int -> TestingResponse
makeResp status ns =
  TestingResponse
    { durationNs = Nanoseconds (fromIntegral ns)
    , statusCode = status
    , respBody = Nothing
    , errorMessage = Nothing
    }
