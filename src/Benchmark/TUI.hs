module Benchmark.TUI (
    runTUI,
    TUIState,
    BenchmarkEvent (..),
    initialState,
) where

import Benchmark.TUI.State
import Benchmark.TUI.Widgets
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel, hBorder)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))

-- | Custom Events for Brick
data CustomEvent
    = Tick
    | BenchEvent BenchmarkEvent

-- | Resource Names
type Name = ()

-- | Drawing
drawUI :: TUIState -> [Widget Name]
drawUI state = [ui]
  where
    ui =
        border $
            vBox
                [ headerSection state
                , hBorder
                , progressSection state
                , hBorder
                , statsSection state
                , hBorder
                , histogramSection state
                , hBorder
                , errorsSection state
                , hBorder
                , footerSection
                ]

headerSection :: TUIState -> Widget Name
headerSection state =
    vBox
        [ txt $ "Target: " <> state ^. tsTarget
        , txt $
            "Endpoint: "
                <> state ^. tsCurrentEndpoint
                <> "  ["
                <> T.pack (show (state ^. tsEndpointIndex))
                <> "/"
                <> T.pack (show (state ^. tsTotalEndpoints))
                <> "]"
        ]

progressSection :: TUIState -> Widget Name
progressSection state =
    let completed = state ^. tsCompleted
        total = state ^. tsIsTotal
        pct =
            if total > 0
                then fromIntegral completed / fromIntegral total
                else 0
        progressText = T.pack $ show completed <> "/" <> show total
     in vBox
            [ progressBar pct "Progress"
            , txt $ "  " <> progressText
            ]

statsSection :: TUIState -> Widget Name
statsSection state = case state ^. tsRollingStats of
    Nothing -> txt "Waiting for data..."
    Just stats ->
        vBox
            [ txt $ "Latency (rolling last " <> T.pack (show rollingWindow) <> ")"
            , txt ""
            , hBox
                [ statBox "Mean" (formatDuration $ stats ^. rsMeanMs)
                , txt "  "
                , statBox "P50" (formatDuration $ stats ^. rsP50Ms)
                , txt "  "
                , statBox "P95" (formatDuration $ stats ^. rsP95Ms)
                , txt "  "
                , statBox "P99" (formatDuration $ stats ^. rsP99Ms)
                ]
            ]
  where
    statBox label value =
        vBox
            [ txt $ T.justifyRight 8 ' ' label
            , txt $ T.justifyRight 8 ' ' value
            ]

histogramSection :: TUIState -> Widget Name
histogramSection state =
    let buckets = state ^. tsBuckets
        labels = ["<5s", "<7.5s", "<10s", "<12.5s", "<15s", ">15s"]
        labeledBuckets = zip labels buckets
     in borderWithLabel (txt " Distribution ") $
            histogram labeledBuckets

errorsSection :: TUIState -> Widget Name
errorsSection state =
    let errors = state ^. tsRecentErrors
        errorCount = state ^. tsErrorCount
        successCount = state ^. tsSuccessCount
        total = errorCount + successCount
        errorPct =
            if total > 0
                then fromIntegral errorCount / fromIntegral total * 100 :: Float
                else 0
     in vBox
            [ txt $
                "Errors: "
                    <> T.pack (show errorCount)
                    <> " ("
                    <> T.pack (show (round errorPct :: Int))
                    <> "%)"
            , if Seq.null errors
                then emptyWidget
                else vBox $ map formatError (toList $ Seq.take 3 errors)
            ]
  where
    formatError (_, msg) = txt $ "  " <> msg
    toList = foldr (:) []

footerSection :: Widget Name
footerSection = hCenter $ txt "q: quit"

-- | Handle events
handleEvent :: BrickEvent Name CustomEvent -> EventM Name TUIState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent (AppEvent Tick) = return ()
handleEvent (AppEvent (BenchEvent event)) = do
    now <- liftIO getCurrentTime
    modify (updateState now event)
    case event of
        BenchmarkFinished -> halt
        _ -> return ()
handleEvent _ = return ()

-- | Attribute map for colors
theAttrMap :: AttrMap
theAttrMap =
    attrMap
        V.defAttr
        [ (attrName "error", fg V.red)
        , (attrName "success", fg V.green)
        , (attrName "progress", fg V.blue)
        ]

-- | The Brick app definition
tuiApp :: App TUIState CustomEvent Name
tuiApp =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theAttrMap
        }

-- | Run the TUI with events from a TChan
runTUI :: TChan BenchmarkEvent -> TUIState -> IO TUIState
runTUI eventChan initialSt = do
    brickChan <- newBChan 100

    -- Fork a thread to forward events from TChan to Brick
    void $ forkIO $ forever $ do
        event <- atomically $ readTChan eventChan
        writeBChan brickChan (BenchEvent event)

    -- Fork a thread for periodic ticks (10 Hz refresh)
    void $ forkIO $ forever $ do
        threadDelay 100_000
        writeBChan brickChan Tick

    -- Build vty and run
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    customMain initialVty buildVty (Just brickChan) tuiApp initialSt
