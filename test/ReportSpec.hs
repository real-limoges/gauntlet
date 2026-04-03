-- | Tests for Benchmark.Report.
module ReportSpec (reportSpec) where

import Benchmark.Report
  ( printBenchmarkReport
  , printMultipleBenchmarkReport
  , printSingleBenchmarkReport
  , printValidationSummary
  )
import Benchmark.Types
  ( BenchmarkStats (..)
  , ComparisonReport (..)
  , MetricRegression (..)
  , RegressionResult (..)
  , ValidationError (..)
  , ValidationSummary (..)
  )
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
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
            , testCase "contains all expected stat labels" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                mapM_
                  (\label -> output `shouldSatisfy` (label `isInfixOf`))
                  ["Mean:", "StdDev:", "p50:", "p95:", "p99:", "ES(p99):", "Min:", "Max:", "Success:"]
            , testCase "shows histogram when stats have histogram data" $ do
                output <- captureStdout $ printSingleBenchmarkReport "ep" stats
                output `shouldSatisfy` ("Distribution:" `isInfixOf`)
            , testCase "hides histogram when stats have no histogram data" $ do
                let emptyHist = stats {histogram = []}
                output <- captureStdout $ printSingleBenchmarkReport "ep" emptyHist
                output `shouldSatisfy` (not . ("Distribution:" `isInfixOf`))
            ]
    , let statsA = mockStats 50.0 5.0
          statsB = mockStats 45.0 4.0
          bayes = mockBayesianComparison
       in testGroup
            "printMultipleBenchmarkReport"
            [ testCase "contains both endpoint names" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport
                        { crNameA = "primary"
                        , crNameB = "candidate"
                        , crStatsA = statsA
                        , crStatsB = statsB
                        , crBayes = bayes
                        }
                output `shouldSatisfy` ("primary" `isInfixOf`)
                output `shouldSatisfy` ("candidate" `isInfixOf`)
            , testCase "contains Bayesian Analysis header" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                output `shouldSatisfy` ("Bayesian Analysis" `isInfixOf`)
            , testCase "contains probability lines" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                output `shouldSatisfy` ("P(b faster than a, means)" `isInfixOf`)
                output `shouldSatisfy` ("P(b faster, single request)" `isInfixOf`)
            , testCase "contains Tail Analysis header" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                output `shouldSatisfy` ("Tail Analysis" `isInfixOf`)
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
          bayes = mockBayesianComparison
       in testGroup
            "printMultipleBenchmarkReport (distribution)"
            [ testCase "shows Distribution header when EMD present" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                output `shouldSatisfy` ("Distribution" `isInfixOf`)
            , testCase "shows Earth Mover's Distance value" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                output `shouldSatisfy` ("Earth Mover" `isInfixOf`)
            , testCase "shows histogram bars for each target" $ do
                output <-
                  captureStdout $
                    printMultipleBenchmarkReport
                      ComparisonReport {crNameA = "a", crNameB = "b", crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
                -- Histogram renders █ bars
                output `shouldSatisfy` ("█" `isInfixOf`)
                output `shouldSatisfy` ("Distribution:" `isInfixOf`)
            ]
    , let statsA = mockStats 50.0 5.0
          statsB = mockStats 45.0 4.0
          namedStats = Map.fromList [("target-a", statsA), ("target-b", statsB)]
          pairs = [("target-a", "target-b", mockBayesianComparison)]
       in testGroup
            "printBenchmarkReport"
            [ testCase "contains Ranking header" $ do
                output <- captureStdout $ printBenchmarkReport namedStats pairs
                output `shouldSatisfy` ("Ranking" `isInfixOf`)
            , testCase "sorts targets by mean in ranking" $ do
                output <- captureStdout $ printBenchmarkReport namedStats pairs
                -- In the ranking section, target-b (mean=45) gets rank 1, target-a (mean=50) gets rank 2
                -- Look for "1" before target-b and "2" before target-a in the ranking lines
                let rankingLines = takeWhile (not . ("Pairwise" `isInfixOf`)) (lines output)
                    hasRank1B = any (\l -> "1" `isInfixOf` l && "target-b" `isInfixOf` l) rankingLines
                    hasRank2A = any (\l -> "2" `isInfixOf` l && "target-a" `isInfixOf` l) rankingLines
                hasRank1B `shouldBe` True
                hasRank2A `shouldBe` True
            , testCase "contains Pairwise Comparisons header" $ do
                output <- captureStdout $ printBenchmarkReport namedStats pairs
                output `shouldSatisfy` ("Pairwise Comparisons" `isInfixOf`)
            ]
    ]
