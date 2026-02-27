module ReportSpec (reportSpec) where

import Benchmark.Report
import Benchmark.Types
import Data.List (isInfixOf)
import Data.Text qualified as T
import Test.Hspec
import TestHelpers (captureStdout, mockBayesianComparison, mockStats)

reportSpec :: Spec
reportSpec = describe "Benchmark.Report" $ do
  describe "printSingleBenchmarkReport" $ do
    let stats = mockStats 50.0 5.0

    it "contains endpoint name" $ do
      output <- captureStdout $ printSingleBenchmarkReport "my-endpoint" stats
      output `shouldSatisfy` ("my-endpoint" `isInfixOf`)

    it "contains Mean stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("Mean:" `isInfixOf`)

    it "contains StdDev stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("StdDev:" `isInfixOf`)

    it "contains p50 stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("p50:" `isInfixOf`)

    it "contains p95 stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("p95:" `isInfixOf`)

    it "contains p99 stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("p99:" `isInfixOf`)

    it "contains ES(p99) stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("ES(p99):" `isInfixOf`)

    it "contains Min stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("Min:" `isInfixOf`)

    it "contains Max stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("Max:" `isInfixOf`)

    it "contains Success stat" $ do
      output <- captureStdout $ printSingleBenchmarkReport "ep" stats
      output `shouldSatisfy` ("Success:" `isInfixOf`)

  describe "printMultipleBenchmarkReport" $ do
    let statsA = mockStats 50.0 5.0
        statsB = mockStats 45.0 4.0
        bayes = mockBayesianComparison

    it "contains both endpoint names" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "primary" "candidate" statsA statsB bayes
      output `shouldSatisfy` ("primary" `isInfixOf`)
      output `shouldSatisfy` ("candidate" `isInfixOf`)

    it "contains Bayesian Analysis header" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
      output `shouldSatisfy` ("Bayesian Analysis" `isInfixOf`)

    it "contains probability lines" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
      output `shouldSatisfy` ("Probability Candidate is Faster" `isInfixOf`)
      output `shouldSatisfy` ("Probability Single Request Faster" `isInfixOf`)

    it "contains Tail Analysis header" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
      output `shouldSatisfy` ("Tail Analysis" `isInfixOf`)

    it "contains Distribution Tests header" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
      output `shouldSatisfy` ("Distribution Tests" `isInfixOf`)

    it "shows 'sample too small' when MWU/KS/AD are Nothing" $ do
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
      output `shouldSatisfy` ("sample too small" `isInfixOf`)

    it "shows 'Significant' when MWU significant" $ do
      let bayesMWU = bayes {mannWhitneyU = Just (MWUResult True)}
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayesMWU
      output `shouldSatisfy` ("Significant" `isInfixOf`)

    it "shows 'Not significant' when MWU not significant" $ do
      let bayesMWU = bayes {mannWhitneyU = Just (MWUResult False)}
      output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayesMWU
      output `shouldSatisfy` ("Not significant" `isInfixOf`)

  describe "printVerifyReport" $ do
    it "shows VERIFICATION TESTS PASSED when all pass" $ do
      let ep = Endpoint "GET" "http://test/api" Nothing [] Nothing
          results = [(ep, [Match, Match])]
      output <- captureStdout $ printVerifyReport results
      output `shouldSatisfy` ("VERIFICATION TESTS PASSED" `isInfixOf`)

    it "shows FAILURE DETAILS when there are failures" $ do
      let ep = Endpoint "POST" "http://test/api" Nothing [] Nothing
          results = [(ep, [StatusMismatch 200 500])]
      output <- captureStdout $ printVerifyReport results
      output `shouldSatisfy` ("FAILURE DETAILS" `isInfixOf`)

    it "shows endpoint method and url in failures" $ do
      let ep = Endpoint "PUT" "http://test/resource" Nothing [] Nothing
          results = [(ep, [StatusMismatch 200 404])]
      output <- captureStdout $ printVerifyReport results
      output `shouldSatisfy` ("PUT" `isInfixOf`)
      output `shouldSatisfy` ("http://test/resource" `isInfixOf`)

    it "shows StatusMismatch text" $ do
      let ep = Endpoint "GET" "http://test" Nothing [] Nothing
          results = [(ep, [StatusMismatch 200 500])]
      output <- captureStdout $ printVerifyReport results
      output `shouldSatisfy` ("Status Mismatch" `isInfixOf`)

    it "shows Body Mismatch with field diffs" $ do
      let ep = Endpoint "GET" "http://test" Nothing [] Nothing
          diff = JsonDiff "data.id" "1" "2"
          results = [(ep, [BodyMismatch [diff]])]
      output <- captureStdout $ printVerifyReport results
      output `shouldSatisfy` ("Body Mismatch" `isInfixOf`)
      output `shouldSatisfy` ("data.id" `isInfixOf`)

  describe "printValidationSummary" $ do
    it "produces no output for empty list" $ do
      output <- captureStdout $ printValidationSummary []
      output `shouldBe` ""

    it "shows Response Validation header for non-empty list" $ do
      let summary = ValidationSummary 10 2 [StatusCodeMismatch 200 500]
      output <- captureStdout $ printValidationSummary [summary]
      output `shouldSatisfy` ("Response Validation" `isInfixOf`)

    it "shows Failed: line when there are failures" $ do
      let summary = ValidationSummary 10 3 [FieldNotFound "$.user.id"]
      output <- captureStdout $ printValidationSummary [summary]
      output `shouldSatisfy` ("Failed:" `isInfixOf`)

    it "shows error details" $ do
      let summary = ValidationSummary 5 1 [FieldNotFound "$.name"]
      output <- captureStdout $ printValidationSummary [summary]
      output `shouldSatisfy` ("$.name" `isInfixOf`)

    it "shows 'and N more' when >10 unique errors" $ do
      let errors = [FieldNotFound ("$.field" <> T.pack (show i)) | i <- [1 :: Int .. 12]]
          summary = ValidationSummary 100 12 errors
      output <- captureStdout $ printValidationSummary [summary]
      output `shouldSatisfy` ("and 2 more" `isInfixOf`)

  describe "printNwayReport" $ do
    let statsA = mockStats 50.0 5.0
        statsB = mockStats 45.0 4.0
        namedStats = [("target-a", statsA), ("target-b", statsB)]
        pairs = [("target-a", "target-b", mockBayesianComparison)]

    it "contains Ranking header" $ do
      output <- captureStdout $ printNwayReport namedStats pairs
      output `shouldSatisfy` ("Ranking" `isInfixOf`)

    it "sorts targets by mean in ranking" $ do
      output <- captureStdout $ printNwayReport namedStats pairs
      -- In the ranking section, target-b (mean=45) gets rank 1, target-a (mean=50) gets rank 2
      -- Look for "1" before target-b and "2" before target-a in the ranking lines
      let rankingLines = takeWhile (not . ("Pairwise" `isInfixOf`)) (lines output)
          hasRank1B = any (\l -> "1" `isInfixOf` l && "target-b" `isInfixOf` l) rankingLines
          hasRank2A = any (\l -> "2" `isInfixOf` l && "target-a" `isInfixOf` l) rankingLines
      hasRank1B `shouldBe` True
      hasRank2A `shouldBe` True

    it "contains Pairwise Comparisons header" $ do
      output <- captureStdout $ printNwayReport namedStats pairs
      output `shouldSatisfy` ("Pairwise Comparisons" `isInfixOf`)
