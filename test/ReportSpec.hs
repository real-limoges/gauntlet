module ReportSpec (reportSpec) where

import Benchmark.Report
import Benchmark.Types
import Data.List (isInfixOf)
import Data.Text qualified as T
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (captureStdout, mockBayesianComparison, mockStats)

reportSpec :: TestTree
reportSpec =
  sequentialTestGroup
    "Benchmark.Report"
    AllFinish
    [ let stats = mockStats 50.0 5.0
       in testGroup
            "printSingleBenchmarkReport"
            [ testCase "contains endpoint name" $ do
                output <- captureStdout $ printSingleBenchmarkReport "my-endpoint" stats
                output `shouldSatisfy` ("my-endpoint" `isInfixOf`)
            , testCase "contains Mean stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("Mean:" `isInfixOf`)
            , testCase "contains StdDev stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("StdDev:" `isInfixOf`)
            , testCase "contains p50 stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("p50:" `isInfixOf`)
            , testCase "contains p95 stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("p95:" `isInfixOf`)
            , testCase "contains p99 stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("p99:" `isInfixOf`)
            , testCase "contains ES(p99) stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("ES(p99):" `isInfixOf`)
            , testCase "contains Min stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("Min:" `isInfixOf`)
            , testCase "contains Max stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("Max:" `isInfixOf`)
            , testCase "contains Success stat" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("Success:" `isInfixOf`)
            ]
    , let statsA = mockStats 50.0 5.0
          statsB = mockStats 45.0 4.0
          bayes = mockBayesianComparison
       in testGroup
            "printMultipleBenchmarkReport"
            [ testCase "contains both endpoint names" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "primary" "candidate" statsA statsB bayes
                output `shouldSatisfy` ("primary" `isInfixOf`)
                output `shouldSatisfy` ("candidate" `isInfixOf`)
            , testCase "contains Bayesian Analysis header" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
                output `shouldSatisfy` ("Bayesian Analysis" `isInfixOf`)
            , testCase "contains probability lines" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
                output `shouldSatisfy` ("Probability Candidate is Faster" `isInfixOf`)
                output `shouldSatisfy` ("Probability Single Request Faster" `isInfixOf`)
            , testCase "contains Tail Analysis header" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
                output `shouldSatisfy` ("Tail Analysis" `isInfixOf`)
            , testCase "contains Distribution Tests header" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
                output `shouldSatisfy` ("Distribution Tests" `isInfixOf`)
            , testCase "shows 'sample too small' when MWU/KS/AD are Nothing" $ do
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayes
                output `shouldSatisfy` ("sample too small" `isInfixOf`)
            , testCase "shows 'Significant' when MWU significant" $ do
                let bayesMWU = bayes {mannWhitneyU = Just (MWUResult True)}
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayesMWU
                output `shouldSatisfy` ("Significant" `isInfixOf`)
            , testCase "shows 'Not significant' when MWU not significant" $ do
                let bayesMWU = bayes {mannWhitneyU = Just (MWUResult False)}
                output <- captureStdout $ printMultipleBenchmarkReport "a" "b" statsA statsB bayesMWU
                output `shouldSatisfy` ("Not significant" `isInfixOf`)
            ]
    , testGroup
        "printVerifyReport"
        [ testCase "shows VERIFICATION TESTS PASSED when all pass" $ do
            let ep = Endpoint "GET" "http://test/api" Nothing [] Nothing
                results = [(ep, [Match, Match])]
            output <- captureStdout $ printVerifyReport results
            output `shouldSatisfy` ("VERIFICATION TESTS PASSED" `isInfixOf`)
        , testCase "shows FAILURE DETAILS when there are failures" $ do
            let ep = Endpoint "POST" "http://test/api" Nothing [] Nothing
                results = [(ep, [StatusMismatch 200 500])]
            output <- captureStdout $ printVerifyReport results
            output `shouldSatisfy` ("FAILURE DETAILS" `isInfixOf`)
        , testCase "shows endpoint method and url in failures" $ do
            let ep = Endpoint "PUT" "http://test/resource" Nothing [] Nothing
                results = [(ep, [StatusMismatch 200 404])]
            output <- captureStdout $ printVerifyReport results
            output `shouldSatisfy` ("PUT" `isInfixOf`)
            output `shouldSatisfy` ("http://test/resource" `isInfixOf`)
        , testCase "shows StatusMismatch text" $ do
            let ep = Endpoint "GET" "http://test" Nothing [] Nothing
                results = [(ep, [StatusMismatch 200 500])]
            output <- captureStdout $ printVerifyReport results
            output `shouldSatisfy` ("Status Mismatch" `isInfixOf`)
        , testCase "shows Body Mismatch with field diffs" $ do
            let ep = Endpoint "GET" "http://test" Nothing [] Nothing
                diff = JsonDiff "data.id" "1" "2"
                results = [(ep, [BodyMismatch [diff]])]
            output <- captureStdout $ printVerifyReport results
            output `shouldSatisfy` ("Body Mismatch" `isInfixOf`)
            output `shouldSatisfy` ("data.id" `isInfixOf`)
        ]
    , testGroup
        "printValidationSummary"
        [ testCase "produces no output for empty list" $ do
            output <- captureStdout $ printValidationSummary []
            output `shouldBe` ""
        , testCase "shows Response Validation header for non-empty list" $ do
            let summary = ValidationSummary 10 2 [StatusCodeMismatch 200 500]
            output <- captureStdout $ printValidationSummary [summary]
            output `shouldSatisfy` ("Response Validation" `isInfixOf`)
        , testCase "shows Failed: line when there are failures" $ do
            let summary = ValidationSummary 10 3 [FieldNotFound "$.user.id"]
            output <- captureStdout $ printValidationSummary [summary]
            output `shouldSatisfy` ("Failed:" `isInfixOf`)
        , testCase "shows error details" $ do
            let summary = ValidationSummary 5 1 [FieldNotFound "$.name"]
            output <- captureStdout $ printValidationSummary [summary]
            output `shouldSatisfy` ("$.name" `isInfixOf`)
        , testCase "shows 'and N more' when >10 unique errors" $ do
            let errors = [FieldNotFound ("$.field" <> T.pack (show i)) | i <- [1 :: Int .. 12]]
                summary = ValidationSummary 100 12 errors
            output <- captureStdout $ printValidationSummary [summary]
            output `shouldSatisfy` ("and 2 more" `isInfixOf`)
        ]
    , let statsA = mockStats 50.0 5.0
          statsB = mockStats 45.0 4.0
          namedStats = [("target-a", statsA), ("target-b", statsB)]
          pairs = [("target-a", "target-b", mockBayesianComparison)]
       in testGroup
            "printNwayReport"
            [ testCase "contains Ranking header" $ do
                output <- captureStdout $ printNwayReport namedStats pairs
                output `shouldSatisfy` ("Ranking" `isInfixOf`)
            , testCase "sorts targets by mean in ranking" $ do
                output <- captureStdout $ printNwayReport namedStats pairs
                -- In the ranking section, target-b (mean=45) gets rank 1, target-a (mean=50) gets rank 2
                -- Look for "1" before target-b and "2" before target-a in the ranking lines
                let rankingLines = takeWhile (not . ("Pairwise" `isInfixOf`)) (lines output)
                    hasRank1B = any (\l -> "1" `isInfixOf` l && "target-b" `isInfixOf` l) rankingLines
                    hasRank2A = any (\l -> "2" `isInfixOf` l && "target-a" `isInfixOf` l) rankingLines
                hasRank1B `shouldBe` True
                hasRank2A `shouldBe` True
            , testCase "contains Pairwise Comparisons header" $ do
                output <- captureStdout $ printNwayReport namedStats pairs
                output `shouldSatisfy` ("Pairwise Comparisons" `isInfixOf`)
            ]
    ]
