{-# LANGUAGE TemplateHaskell #-}

module Benchmark.TUI.State
  ( -- * Types
    TUIState (..)
  , BenchmarkEvent (..)
  , RollingStats (..)

    -- * Functions
  , initialState
  , updateState
  , rollingWindow

    -- * TUIState lenses
  , tsTarget
  , tsCurrentEndpoint
  , tsEndpointIndex
  , tsTotalEndpoints
  , tsCompleted
  , tsIsTotal
  , tsSuccessCount
  , tsErrorCount
  , tsStartTime
  , tsRecentDurations
  , tsRecentErrors
  , tsRecentRequests
  , tsRollingStats
  , tsFinished
  , tsStatus
  , tsElapsedSecs

    -- * RollingStats lenses
  , rsMeanMs
  , rsP50Ms
  , rsP95Ms
  , rsP99Ms
  , rsMinMs
  , rsMaxMs
  ) where

import Benchmark.Types (BenchmarkStats (..), Milliseconds (..), Nanoseconds, nsToMs)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time (UTCTime)
import Lens.Micro.TH (makeLenses)
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
  | BenchmarkFinished
  deriving (Show, Eq)

data RollingStats = RollingStats
  { _rsMeanMs :: Double
  , _rsP50Ms :: Double
  , _rsP95Ms :: Double
  , _rsP99Ms :: Double
  , _rsMinMs :: Double
  , _rsMaxMs :: Double
  }
  deriving (Show, Eq)

makeLenses ''RollingStats

data TUIState = TUIState
  { _tsTarget :: Text
  , _tsCurrentEndpoint :: Text
  , _tsEndpointIndex :: Int
  , _tsTotalEndpoints :: Int
  , _tsCompleted :: Int
  , _tsIsTotal :: Int
  , _tsSuccessCount :: Int
  , _tsErrorCount :: Int
  , _tsStartTime :: Maybe UTCTime
  , _tsRecentDurations :: Seq Double
  , _tsRecentErrors :: Seq (UTCTime, Text)
  , _tsRecentRequests :: Seq Bool
  , _tsRollingStats :: Maybe RollingStats
  , _tsFinished :: Bool
  , _tsStatus :: Text
  , _tsElapsedSecs :: Double
  }
  deriving (Show, Eq)

makeLenses ''TUIState

initialState :: Text -> Int -> Int -> TUIState
initialState target total endpoints =
  TUIState
    { _tsTarget = target
    , _tsCurrentEndpoint = ""
    , _tsEndpointIndex = 0
    , _tsTotalEndpoints = endpoints
    , _tsCompleted = 0
    , _tsIsTotal = total
    , _tsSuccessCount = 0
    , _tsErrorCount = 0
    , _tsStartTime = Nothing
    , _tsRecentDurations = Seq.empty
    , _tsRecentErrors = Seq.empty
    , _tsRecentRequests = Seq.empty
    , _tsRollingStats = Nothing
    , _tsFinished = False
    , _tsStatus = ""
    , _tsElapsedSecs = 0
    }

updateState :: UTCTime -> BenchmarkEvent -> TUIState -> TUIState
updateState now event state = case event of
  RequestCompleted durationNs statusCode ->
    let ms = unMilliseconds $ nsToMs durationNs
        newDurations = addToRolling ms (_tsRecentDurations state)
        newStats = computeRollingStats newDurations
        isSuccess = statusCode >= 200 && statusCode < 400
     in state
          { _tsCompleted = _tsCompleted state + 1
          , _tsSuccessCount = _tsSuccessCount state + (if isSuccess then 1 else 0)
          , _tsErrorCount = _tsErrorCount state + (if isSuccess then 0 else 1)
          , _tsRecentDurations = newDurations
          , _tsRecentRequests = addToTimeline isSuccess (_tsRecentRequests state)
          , _tsRollingStats = Just newStats
          , _tsStartTime = _tsStartTime state <|> Just now
          }
  RequestFailed err ->
    let newErrors = addToRolling (now, err) (_tsRecentErrors state)
     in state
          { _tsCompleted = _tsCompleted state + 1
          , _tsErrorCount = _tsErrorCount state + 1
          , _tsRecentErrors = Seq.take 5 newErrors
          , _tsRecentRequests = addToTimeline False (_tsRecentRequests state)
          }
  EndpointStarted endpoint index total ->
    state
      { _tsCurrentEndpoint = endpoint
      , _tsEndpointIndex = index
      , _tsTotalEndpoints = total
      }
  StatusMessage msg ->
    state {_tsStatus = msg}
  PhaseStarted total ->
    state
      { _tsCompleted = 0
      , _tsIsTotal = total
      , _tsSuccessCount = 0
      , _tsErrorCount = 0
      , _tsStartTime = Nothing
      , _tsElapsedSecs = 0
      , _tsRollingStats = Nothing
      , _tsRecentDurations = Seq.empty
      , _tsRecentRequests = Seq.empty
      }
  BenchmarkFinished ->
    state {_tsFinished = True}
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
            _ -> (head sorted, last sorted)
       in RollingStats
            { _rsMeanMs = avg
            , _rsP50Ms = percentileList 0.50 sorted
            , _rsP95Ms = percentileList 0.95 sorted
            , _rsP99Ms = percentileList 0.99 sorted
            , _rsMinMs = mn
            , _rsMaxMs = mx
            }
