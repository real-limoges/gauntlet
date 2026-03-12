-- | Tests for Benchmark.Report.Formatting.
module FormattingSpec (formattingSpec) where

import Benchmark.Report.Formatting (formatValidationError)
import Benchmark.Types (ValidationError (..))
import Data.Aeson (Value (..))
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

formattingSpec :: TestTree
formattingSpec =
  testGroup
    "Benchmark.Report.Formatting"
    [ testGroup
        "formatValidationError"
        [ testCase "StatusCodeMismatch" $
            formatValidationError (StatusCodeMismatch 200 500)
              `shouldBe` "Status code mismatch: expected 200, got 500"
        , testCase "FieldNotFound" $
            formatValidationError (FieldNotFound "$.name")
              `shouldBe` "Field not found: $.name"
        , testCase "FieldValueMismatch" $
            formatValidationError (FieldValueMismatch "$.id" (Number 1) (Number 2))
              `shouldBe` "Field value mismatch at $.id: expected 1, actual 2"
        , testCase "BodyAbsent" $
            formatValidationError BodyAbsent
              `shouldBe` "Response body absent"
        , testCase "BodyInvalidJSON" $
            formatValidationError BodyInvalidJSON
              `shouldBe` "Response body is not valid JSON"
        ]
    ]
