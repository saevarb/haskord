module Haskord.Rendering where

import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL (toStrict, unpack, unlines)
import qualified Data.Vector            as V
import Data.Maybe

import Graphics.Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.BChan
import           Text.Pretty.Simple

data Screen
    = LogList
    | ErrList
    | Chat
    deriving (Ord, Eq, Show)


data RenderEvent
    = ErrorAdded (Text, Text)
    | MessageAdded (Text, Text)
    deriving (Show, Eq, Ord)

data RenderingState
    = RenderingState
    { logMessages :: List Screen (Text, Text)
    , errMessages :: List Screen (Text, Text)
    , screen      :: Screen
    }

data Tab
    = Tab
    { tabName     :: Text
    , tabRenderer :: RenderingState -> Widget Screen
    }

tabs :: [(Screen, Tab)]
tabs =
    [ (LogList, Tab "Messages" renderLog)
    , (ErrList, Tab "Errors" renderLog)
    , (Chat, Tab "Chat" renderChat)
    ]

initialRenderingState :: RenderingState
initialRenderingState =
    RenderingState
    { errMessages = list ErrList V.empty 1
    , logMessages = list LogList V.empty 1
    , screen = LogList
    }


renderInterface :: BChan RenderEvent -> IO ()
renderInterface bchan =
    () <$ customMain (mkVty defaultConfig) (Just bchan) brickApp initialRenderingState

renderChat :: RenderingState -> Widget Screen
renderChat _ =
    border $ vCenter $ hCenter $ str "This is the chat"

renderLog :: RenderingState -> Widget Screen
renderLog s =
    border (renderList renderCurElem True msgs)
    <+> border (vCenter . hCenter $ renderContent msgs)
  where
    msgs =
      case screen s of
            LogList -> logMessages s
            ErrList -> errMessages s
            _ -> error "Unreachable"
    renderCurElem True (e, _) =
        withAttr "selected" $ txt e
    renderCurElem _ (e, _) = txt e
    renderContent ls =
        case listSelectedElement ls of
            Nothing -> txt "No Content"
            Just (_, (_, msg)) -> txtWrap msg

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
myAttrMap _ = attrMap Graphics.Vty.defAttr [("selected", fg cyan)]

eventHandler
  :: RenderingState
     -> BrickEvent n RenderEvent -> EventM Screen (Next RenderingState)
eventHandler s (VtyEvent (EvKey (KChar 'q') _)) = do
    halt s
eventHandler s (VtyEvent (EvKey (KChar '1') _)) = do
    continue s { screen = LogList }
eventHandler s (VtyEvent (EvKey (KChar '2') _)) = do
    continue s { screen = ErrList }
eventHandler s (VtyEvent (EvKey (KChar '3') _)) = do
    continue s { screen = Chat }
eventHandler s (VtyEvent event) = do
    newList <- handleListEventVi handleListEvent event (logMessages s)
    continue $ s { logMessages = newList }
eventHandler s (AppEvent (MessageAdded msg)) = do
    continue $ s { logMessages = listInsert 0 msg (logMessages s)}
eventHandler s _ =
    continue s


renderAll :: RenderingState -> [Widget Screen]
renderAll s =
    let curTab = fromJust $ lookup (screen s) tabs
    in [renderTabs (screen s) <=> tabRenderer curTab s]


