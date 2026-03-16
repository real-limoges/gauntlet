-- | TUI state types, event definitions, and state update logic.
module Benchmark.TUI.State
  ( -- * Types
    TUIState (..)
  , BenchmarkEvent (..)
  , RollingStats (..)

    -- * Functions
  , initialState
  , updateState
  , rollingWindow
  ) where

import Benchmark.Types (Milliseconds (..), Nanoseconds, nsToMs)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time (UTCTime)
import Stats.Common (percentileList)

-- | Size of rolling window
rollingWindow :: Int
rollingWindow = 100

-- | Events emitted by runner
data BenchmarkEvent
  = RequestCompleted Nanoseconds Int
  | RequestFailed Text
  | EndpointStarted Text Int Int
  | StatusMessage Text
  | PhaseStarted Int
  | TargetStarted Text Int Int
  | CurrentRpsUpdated Double
  | LoadStepChanged Int Double
  | BenchmarkFinished
  | BenchmarkFailed Text
  deriving (Show, Eq)

-- | Descriptive statistics computed over the rolling window of recent requests.
data RollingStats = RollingStats
  { rsMeanMs :: Double
  , rsP50Ms :: Double
  , rsP95Ms :: Double
  , rsP99Ms :: Double
  , rsMinMs :: Double
  , rsMaxMs :: Double
  }
  deriving (Show, Eq)

-- | Mutable state for the Brick TUI, updated on each 'BenchmarkEvent'.
data TUIState = TUIState
  { tsTarget :: Text
  , tsCurrentEndpoint :: Text
  , tsEndpointIndex :: Int
  , tsTotalEndpoints :: Int
  , tsCompleted :: Int
  , tsTotalIterations :: Int
  , tsSuccessCount :: Int
  , tsErrorCount :: Int
  , tsStartTime :: Maybe UTCTime
  , tsRecentDurations :: Seq Double
  , tsRecentErrors :: Seq (UTCTime, Text)
  , tsRecentRequests :: Seq Bool
  , tsRollingStats :: Maybe RollingStats
  , tsFinished :: Bool
  , tsError :: Maybe Text
  , tsStatus :: Text
  , tsElapsedSecs :: Double
  , tsCurrentRps :: Maybe Double
  , tsTargetRps :: Maybe Double
  , tsCurrentStep :: Maybe Int
  }
  deriving (Show, Eq)

-- | Construct an initial 'TUIState' with zero progress counters.
initialState :: Text -> Int -> Int -> TUIState
initialState target total endpoints =
  TUIState
    { tsTarget = target
    , tsCurrentEndpoint = ""
    , tsEndpointIndex = 0
    , tsTotalEndpoints = endpoints
    , tsCompleted = 0
    , tsTotalIterations = total
    , tsSuccessCount = 0
    , tsErrorCount = 0
    , tsStartTime = Nothing
    , tsRecentDurations = Seq.empty
    , tsRecentErrors = Seq.empty
    , tsRecentRequests = Seq.empty
    , tsRollingStats = Nothing
    , tsFinished = False
    , tsError = Nothing
    , tsStatus = ""
    , tsElapsedSecs = 0
    , tsCurrentRps = Nothing
    , tsTargetRps = Nothing
    , tsCurrentStep = Nothing
    }

-- | Apply a 'BenchmarkEvent' to the TUI state, updating counters and rolling stats.
updateState :: UTCTime -> BenchmarkEvent -> TUIState -> TUIState
updateState now event state = case event of
  RequestCompleted durationNs statusCode ->
    let ms = unMilliseconds $ nsToMs durationNs
        newDurations = addToRolling ms (tsRecentDurations state)
        newStats = computeRollingStats newDurations
        isSuccess = statusCode >= 200 && statusCode < 400
     in state
          { tsCompleted = tsCompleted state + 1
          , tsSuccessCount = tsSuccessCount state + (if isSuccess then 1 else 0)
          , tsErrorCount = tsErrorCount state + (if isSuccess then 0 else 1)
          , tsRecentDurations = newDurations
          , tsRecentRequests = addToTimeline isSuccess (tsRecentRequests state)
          , tsRollingStats = Just newStats
          , tsStartTime = tsStartTime state <|> Just now
          }
  RequestFailed err ->
    let newErrors = addToRolling (now, err) (tsRecentErrors state)
     in state
          { tsCompleted = tsCompleted state + 1
          , tsErrorCount = tsErrorCount state + 1
          , tsRecentErrors = Seq.take 5 newErrors
          , tsRecentRequests = addToTimeline False (tsRecentRequests state)
          }
  EndpointStarted endpoint index total ->
    state
      { tsCurrentEndpoint = endpoint
      , tsEndpointIndex = index
      , tsTotalEndpoints = total
      }
  StatusMessage msg ->
    state {tsStatus = msg}
  PhaseStarted total ->
    state
      { tsCompleted = 0
      , tsTotalIterations = total
      , tsSuccessCount = 0
      , tsErrorCount = 0
      , tsStartTime = Nothing
      , tsElapsedSecs = 0
      , tsRollingStats = Nothing
      , tsRecentDurations = Seq.empty
      , tsRecentRequests = Seq.empty
      }
  TargetStarted name _idx total ->
    state
      { tsTarget = name
      , tsCompleted = 0
      , tsTotalIterations = total
      , tsSuccessCount = 0
      , tsErrorCount = 0
      , tsStartTime = Nothing
      , tsElapsedSecs = 0
      , tsRollingStats = Nothing
      , tsRecentDurations = Seq.empty
      , tsRecentRequests = Seq.empty
      , tsCurrentRps = Nothing
      , tsTargetRps = Nothing
      , tsCurrentStep = Nothing
      }
  CurrentRpsUpdated rps ->
    state {tsCurrentRps = Just rps}
  LoadStepChanged step rps ->
    state {tsCurrentStep = Just step, tsTargetRps = Just rps}
  BenchmarkFinished ->
    state {tsFinished = True}
  BenchmarkFailed msg ->
    state {tsFinished = True, tsError = Just msg}
  where
    unMilliseconds (Milliseconds ms) = ms

addToRolling :: a -> Seq a -> Seq a
addToRolling x xs
  | Seq.length xs >= rollingWindow = x Seq.<| Seq.deleteAt (Seq.length xs - 1) xs
  | otherwise = x Seq.<| xs

-- | Append to timeline, dropping oldest when over capacity (newest at end)
addToTimeline :: a -> Seq a -> Seq a
addToTimeline x xs
  | Seq.length xs >= timelineCapacity = Seq.deleteAt 0 xs Seq.|> x
  | otherwise = xs Seq.|> x
  where
    timelineCapacity = 80

computeRollingStats :: Seq Double -> RollingStats
computeRollingStats durations
  | Seq.null durations = RollingStats 0 0 0 0 0 0
  | otherwise =
      let sorted = toList (Seq.sort durations)
          n = length sorted
          avg = sum sorted / fromIntegral n
          (mn, mx) = case sorted of
            [] -> (0, 0)
            (lo_ : _) -> (lo_, last sorted)
       in RollingStats
            { rsMeanMs = avg
            , rsP50Ms = percentileList 0.50 sorted
            , rsP95Ms = percentileList 0.95 sorted
            , rsP99Ms = percentileList 0.99 sorted
            , rsMinMs = mn
            , rsMaxMs = mx
            }
