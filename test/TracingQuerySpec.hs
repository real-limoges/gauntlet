module TracingQuerySpec (tracingQuerySpec) where

import Data.Text qualified as T
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Tracing.Query (buildTraceQL)
import Tracing.Types (Nanoseconds (..), TraceQuery (..))

tracingQuerySpec :: TestTree
tracingQuerySpec =
  testGroup
    "Tracing.Query.buildTraceQL"
    [ testCase "always includes the service name" $ do
        let q = baseQuery {queryService = "my-service"}
        let result = buildTraceQL q
        result `shouldSatisfy` T.isInfixOf "my-service"
    , testCase "wraps output in curly braces" $ do
        let result = buildTraceQL baseQuery
        T.isPrefixOf "{" result `shouldBe` True
        T.isSuffixOf "}" result `shouldBe` True
    , testCase "uses resource.service.name key" $ do
        let result = buildTraceQL baseQuery
        result `shouldSatisfy` T.isInfixOf "resource.service.name"
    , testCase "includes span name when Just" $ do
        let q = baseQuery {querySpanName = Just "GET /api/users"}
        let result = buildTraceQL q
        result `shouldSatisfy` T.isInfixOf "GET /api/users"
        result `shouldSatisfy` T.isInfixOf "name="
    , testCase "excludes span name when Nothing" $ do
        let q = baseQuery {querySpanName = Nothing}
        let result = buildTraceQL q
        -- span name would appear as `name="..."` (no dot-prefix)
        -- service name appears as `resource.service.name="..."` - won't false-match
        result `shouldBe` "{resource.service.name=\"test-service\"}"
    , testCase "includes min duration when Just" $ do
        let q = baseQuery {queryMinDuration = Just "100ms"}
        let result = buildTraceQL q
        result `shouldSatisfy` T.isInfixOf "100ms"
        result `shouldSatisfy` T.isInfixOf "duration>"
    , testCase "excludes min duration when Nothing" $ do
        let q = baseQuery {queryMinDuration = Nothing}
        let result = buildTraceQL q
        result `shouldSatisfy` (not . T.isInfixOf "duration>")
    , testCase "joins multiple conditions with &&" $ do
        let q = baseQuery {querySpanName = Just "op", queryMinDuration = Just "50ms"}
        let result = buildTraceQL q
        result `shouldSatisfy` T.isInfixOf " && "
    , testCase "single condition has no && separator" $ do
        -- Only service name (no span name, no min duration)
        let result = buildTraceQL baseQuery
        result `shouldSatisfy` (not . T.isInfixOf " && ")
    , testCase "three conditions include all fields" $ do
        let q = baseQuery {querySpanName = Just "GET /api", queryMinDuration = Just "100ms"}
        let result = buildTraceQL q
        result `shouldSatisfy` T.isInfixOf "resource.service.name=\"test-service\""
        result `shouldSatisfy` T.isInfixOf "name=\"GET /api\""
        result `shouldSatisfy` T.isInfixOf "duration>100ms"
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

baseQuery :: TraceQuery
baseQuery =
  TraceQuery
    { queryService = "test-service"
    , querySpanName = Nothing
    , queryStartNs = Nanoseconds 0
    , queryEndNs = Nanoseconds 1_000_000_000
    , queryMinDuration = Nothing
    }
