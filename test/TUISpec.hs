module TUISpec (tuiStateSpec, tuiWidgetsSpec) where

import Benchmark.TUI.State
import Benchmark.TUI.Widgets
import Benchmark.Types (Nanoseconds (..))
import Data.Sequence qualified as Seq
import Data.Time (addUTCTime, getCurrentTime)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tuiStateSpec :: TestTree
tuiStateSpec =
  testGroup
    "Benchmark.TUI.State"
    [ testGroup
        "initialState"
        [ testCase "creates state with correct target, totals, and zeroed counters" $ do
            let state = initialState "http://test.com" 100 5
            _tsTarget state `shouldBe` "http://test.com"
            _tsIsTotal state `shouldBe` 100
            _tsTotalEndpoints state `shouldBe` 5
            _tsCompleted state `shouldBe` 0
            _tsSuccessCount state `shouldBe` 0
            _tsErrorCount state `shouldBe` 0
            _tsFinished state `shouldBe` False
        , testCase "starts with empty rolling stats and durations" $ do
            let state = initialState "http://test.com" 100 5
            _tsRollingStats state `shouldBe` Nothing
            Seq.null (_tsRecentDurations state) `shouldBe` True
        ]
    , testGroup
        "updateState with RequestCompleted"
        ( [ testCase "increments completed count" $ do
              now <- getCurrentTime
              let state = initialState "http://test.com" 100 5
                  event = RequestCompleted (Nanoseconds 50_000_000) 200
                  newState = updateState now event state
              _tsCompleted newState `shouldBe` 1
          , testCase "updates rolling stats" $ do
              now <- getCurrentTime
              let state = initialState "http://test.com" 100 5
                  event = RequestCompleted (Nanoseconds 50_000_000) 200
                  newState = updateState now event state
              _tsRollingStats newState `shouldSatisfy` (/= Nothing)
          , testCase "adds duration to recent durations" $ do
              now <- getCurrentTime
              let state = initialState "http://test.com" 100 5
                  event = RequestCompleted (Nanoseconds 50_000_000) 200
                  newState = updateState now event state
              Seq.length (_tsRecentDurations newState) `shouldBe` 1
          , testCase "sets start time on first request" $ do
              now <- getCurrentTime
              let state = initialState "http://test.com" 100 5
                  event = RequestCompleted (Nanoseconds 50_000_000) 200
                  newState = updateState now event state
              _tsStartTime newState `shouldBe` Just now
          , testCase "does not change start time on subsequent requests" $ do
              now <- getCurrentTime
              let later = addUTCTime 1 now
                  state = initialState "http://test.com" 100 5
                  event1 = RequestCompleted (Nanoseconds 50_000_000) 200
                  event2 = RequestCompleted (Nanoseconds 60_000_000) 200
                  state1 = updateState now event1 state
                  state2 = updateState later event2 state1
              _tsStartTime state2 `shouldBe` Just now
          ]
            ++ [ testCase ("categorizes status " ++ show (code :: Int) ++ " correctly") $ do
                   now <- getCurrentTime
                   let state = initialState "http://test.com" 100 5
                       event = RequestCompleted (Nanoseconds 50_000_000) code
                       newState = updateState now event state
                   _tsSuccessCount newState `shouldBe` expSuccess
                   _tsErrorCount newState `shouldBe` expError
               | (code, expSuccess, expError) <- [(200, 1, 0), (301, 1, 0), (404, 0, 1), (500, 0, 1)]
               ]
        )
    , testGroup
        "updateState with RequestFailed"
        [ testCase "increments completed and error count" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestFailed "Connection timeout"
                newState = updateState now event state
            _tsCompleted newState `shouldBe` 1
            _tsErrorCount newState `shouldBe` 1
            _tsSuccessCount newState `shouldBe` 0
        , testCase "adds error to recent errors" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestFailed "Connection timeout"
                newState = updateState now event state
            Seq.length (_tsRecentErrors newState) `shouldBe` 1
        , testCase "keeps only last 5 errors" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                addError s = updateState now (RequestFailed "error") s
                finalState = iterate addError state !! 10
            Seq.length (_tsRecentErrors finalState) `shouldBe` 5
        ]
    , testGroup
        "updateState with EndpointStarted"
        [ testCase "updates current endpoint" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = EndpointStarted "/api/users" 2 5
                newState = updateState now event state
            _tsCurrentEndpoint newState `shouldBe` "/api/users"
            _tsEndpointIndex newState `shouldBe` 2
            _tsTotalEndpoints newState `shouldBe` 5
        ]
    , testGroup
        "updateState with TargetStarted"
        [ testCase "resets counters and sets target name" $ do
            now <- getCurrentTime
            let state = initialState "old" 100 5
                event = TargetStarted "new-target" 2 50
                newState = updateState now event state
            _tsTarget newState `shouldBe` "new-target"
            _tsCompleted newState `shouldBe` 0
            _tsIsTotal newState `shouldBe` 50
            _tsSuccessCount newState `shouldBe` 0
            _tsErrorCount newState `shouldBe` 0
            _tsStartTime newState `shouldBe` Nothing
            _tsRollingStats newState `shouldBe` Nothing
            Seq.null (_tsRecentDurations newState) `shouldBe` True
        , testCase "resets after requests have been recorded" $ do
            now <- getCurrentTime
            let state = initialState "old" 100 5
                withRequests = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) state
                newState = updateState now (TargetStarted "new-target" 1 75) withRequests
            _tsCompleted newState `shouldBe` 0
            _tsSuccessCount newState `shouldBe` 0
            _tsRollingStats newState `shouldBe` Nothing
        ]
    , testGroup
        "updateState with BenchmarkFinished"
        [ testCase "sets finished flag" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = BenchmarkFinished
                newState = updateState now event state
            _tsFinished newState `shouldBe` True
        ]
    , testGroup
        "rolling window"
        [ testCase "rollingWindow is 100" $
            rollingWindow `shouldBe` 100
        , testCase "limits durations to rolling window size" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 200 1
                addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                finalState = iterate addRequest state !! 150
            Seq.length (_tsRecentDurations finalState) `shouldBe` 100
        ]
    ]

tuiWidgetsSpec :: TestTree
tuiWidgetsSpec =
  testGroup
    "Benchmark.TUI.Widgets"
    [ testGroup
        "formatDuration"
        [ testCase "formats microseconds for <1ms" $
            formatDuration 0.5 `shouldBe` "500µs"
        , testCase "formats milliseconds for 1-1000ms" $ do
            formatDuration 50 `shouldBe` "50ms"
            formatDuration 999 `shouldBe` "999ms"
        , testCase "formats seconds for >1000ms" $ do
            formatDuration 1500 `shouldBe` "1.5s"
            formatDuration 5000 `shouldBe` "5.0s"
        , testCase "handles zero" $
            formatDuration 0 `shouldBe` "0µs"
        ]
    , testGroup
        "formatRPS"
        [ testCase "formats low RPS with decimal" $
            formatRPS 0.5 `shouldBe` "0.5 rps"
        , testCase "formats very low RPS" $
            formatRPS 0.05 `shouldBe` "<0.1 rps"
        , testCase "formats integer RPS" $ do
            formatRPS 42.3 `shouldBe` "42 rps"
            formatRPS 100 `shouldBe` "100 rps"
        ]
    , testGroup
        "formatElapsed"
        [ testCase "formats seconds as MM:SS" $ do
            formatElapsed 65 `shouldBe` "01:05"
            formatElapsed 0 `shouldBe` "00:00"
            formatElapsed 59 `shouldBe` "00:59"
        , testCase "formats minutes correctly" $ do
            formatElapsed 125 `shouldBe` "02:05"
            formatElapsed 600 `shouldBe` "10:00"
        , testCase "formats hours as HH:MM:SS" $ do
            formatElapsed 3661 `shouldBe` "01:01:01"
            formatElapsed 7200 `shouldBe` "02:00:00"
        , testCase "handles negative values" $
            formatElapsed (-10) `shouldBe` "00:00"
        ]
    ]
