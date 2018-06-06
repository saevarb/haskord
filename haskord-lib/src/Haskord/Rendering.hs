{-# LANGUAGE OverloadedStrings #-}
module Haskord.Rendering
    ( BChan
    , RenderEvent (..)
    , renderInterface
    , newBChan
    , writeBChan
    )where

import           Data.Bool
import           Data.Char
import           Data.Maybe
import qualified Data.Vector          as V

import           Brick
import           Brick.BChan
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Graphics.Vty
import           Streaming            as S

import           Haskord.Logging
import           Haskord.Prelude

data Screen
    = Log
    | Chat
    deriving (Ord, Eq, Show)


data RenderEvent
    = MessageAdded LogMessage
    deriving (Show)

data RenderingState
    = RenderingState
    { logMessages :: List Screen LogMessage
    , screen      :: Screen
    , logV        :: TVar (BoundedLog LogMessage)
    }

data Tab
    = Tab
    { tabName     :: Text
    , tabRenderer :: RenderingState -> Widget Screen
    }

tabs :: [(Screen, Tab)]
tabs =
    [ (Log, Tab "Messages" renderLog)
    , (Chat, Tab "Chat" renderChat)
    ]

initialRenderingState :: TVar (BoundedLog LogMessage) -> RenderingState
initialRenderingState var =
    RenderingState
    { logMessages = list Log V.empty 1
    , screen = Log
    , logV = var
    }


renderInterface :: TVar (BoundedLog LogMessage) -> BChan RenderEvent -> IO ()
renderInterface lv bchan =
    void $ customMain (mkVty defaultConfig) (Just bchan) brickApp (initialRenderingState lv)

renderChat :: RenderingState -> Widget Screen
renderChat _ =
    border $ vCenter $ hCenter $ str "This is the chat"

renderLog :: RenderingState -> Widget Screen
renderLog RenderingState {..} =
    border (renderList renderCurElem True logMessages)
    <+> renderContent logMessages
  where
    renderCurElem selected LogMessage {..} =
        let fn = bool id (withAttr "selected") selected
        in fn $ (withAttr (attrName $ sevToString severity) $ txt $ prefix severity) <+> txtWrap title
    prefix Info    = " ++ "
    prefix Warning = " !! "
    prefix Error   = " ***** "
    prefix Fatal   = " -- FATAL ERROR -- "
    sevToString = map toLower . show
    renderContent ls =
        case listSelectedElement ls of
            Nothing -> emptyWidget
            Just (_, p) ->
                maybe emptyWidget (border . vCenter . hCenter . txtWrap) $ renderPayload p

renderTabs :: Screen -> Widget Screen
renderTabs current =
    hBox $ map renderTab tabs
  where
    renderTab (screen, tab)
        | current == screen = withAttr "selected" $ border . txt $ tabName tab
        | otherwise = border . txt $ tabName tab

brickApp :: App RenderingState RenderEvent Screen
brickApp =
    App
    { appDraw = renderAll
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = eventHandler
    , appStartEvent = startEvent
    , appAttrMap = myAttrMap
    }

startEvent :: a -> EventM Screen a
startEvent = return

myAttrMap :: p -> AttrMap
myAttrMap _ = attrMap Graphics.Vty.defAttr attrs
  where
    attrs =
        [ ("selected", fg cyan)
        , ("error", fg red)
        , ("warning", fg yellow)
        , ("info", fg blue)
        , ("fatal", fg magenta)
        ]

eventHandler
  :: RenderingState -> BrickEvent n RenderEvent -> EventM Screen (Next RenderingState)
eventHandler s (VtyEvent (EvKey (KChar 'q') _)) =
    halt s
eventHandler s (VtyEvent (EvKey (KChar '1') _)) =
    continue s { screen = Log }
eventHandler s (VtyEvent (EvKey (KChar '2') _)) =
    continue s { screen = Chat }
eventHandler s@RenderingState {..} (VtyEvent event) = do
    newList <- handleListEventVi handleListEvent event logMessages
    continue $ s { logMessages = newList }
eventHandler s@RenderingState {..} (AppEvent (MessageAdded m)) = do
    let newList = listInsert 0 m logMessages
    continue $ s { logMessages = newList }
eventHandler s _ = continue s


renderAll :: RenderingState -> [Widget Screen]
renderAll s =
    let curTab = fromJust $ lookup (screen s) tabs
    in [renderTabs (screen s) <=> tabRenderer curTab s]


