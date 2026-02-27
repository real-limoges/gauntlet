module TracingReportSpec (tracingReportSpec) where

import Benchmark.Types (Milliseconds (..))
import Data.List (isInfixOf)
import Test.Hspec
import TestHelpers (captureStdout)
import Tracing.Report (formatSpanRow, printSpanTable)
import Tracing.Types (SpanAggregation (..))

tracingReportSpec :: Spec
tracingReportSpec = describe "Tracing.Report" $ do
  describe "formatSpanRow" $ do
    let agg =
          SpanAggregation
            { aggSpanName = "GET /api/users"
            , aggCount = 42
            , aggMeanMs = Milliseconds 12.345
            , aggStdDevMs = Milliseconds 1.5
            , aggP50Ms = Milliseconds 11.0
            , aggP95Ms = Milliseconds 18.5
            , aggP99Ms = Milliseconds 25.75
            , aggMinMs = Milliseconds 5.0
            , aggMaxMs = Milliseconds 30.0
            }
        row = formatSpanRow agg

    it "contains span name" $
      row `shouldSatisfy` ("GET /api/users" `isInfixOf`)

    it "contains count" $
      row `shouldSatisfy` ("42" `isInfixOf`)

    it "formats decimal values with 2 decimal places" $
      row `shouldSatisfy` ("12.34" `isInfixOf`)

    it "truncates span names longer than 50 characters" $ do
      let longAgg = agg {aggSpanName = "This is a very long span name that exceeds fifty characters easily"}
          longRow = formatSpanRow longAgg
      longRow `shouldSatisfy` ("This is a very long span name that exceeds fifty " `isInfixOf`)

  describe "printSpanTable" $ do
    it "prints 'No spans to display.' for empty list" $ do
      output <- captureStdout (printSpanTable [])
      output `shouldSatisfy` ("No spans to display." `isInfixOf`)

    it "prints header for non-empty list" $ do
      let agg =
            SpanAggregation
              { aggSpanName = "test"
              , aggCount = 1
              , aggMeanMs = Milliseconds 1.0
              , aggStdDevMs = Milliseconds 0.1
              , aggP50Ms = Milliseconds 1.0
              , aggP95Ms = Milliseconds 1.5
              , aggP99Ms = Milliseconds 2.0
              , aggMinMs = Milliseconds 0.5
              , aggMaxMs = Milliseconds 3.0
              }
      output <- captureStdout (printSpanTable [agg])
      output `shouldSatisfy` ("Span Duration Statistics" `isInfixOf`)
