module Rendering where

import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL (toStrict, unpack, unlines)
import qualified Data.Vector            as V

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


brickApp :: App RenderingState RenderEvent Screen
brickApp =
    App
    { appDraw = render
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = eventHandler
    , appStartEvent = startEvent
    , appAttrMap = myAttrMap
    }
  where
    startEvent = return
    myAttrMap _ = attrMap Graphics.Vty.defAttr [("selected", bg white)]
    eventHandler s (VtyEvent (EvKey (KChar 'q') _)) = do
        halt s
    eventHandler s (VtyEvent event) = do
        newList <- handleListEvent event (logMessages s)
        continue $ s { logMessages = newList }
    eventHandler s (AppEvent (MessageAdded msg)) = do
        continue $ s { logMessages = listInsert 0 msg (logMessages s)}
    eventHandler s _ =
        continue s

    render :: RenderingState -> [Widget Screen]
    render s =
        [renderScreen (screen s) s]

    renderScreen Chat s =
        border $ vCenter $ hCenter $ str "This is the chat"
    renderScreen x s =
        let msgs = case x of
                    LogList -> logMessages s
                    ErrList -> errMessages s
        in border (renderList renderCurElem True msgs)
           <+> border (vCenter . hCenter $ renderContent msgs)
    renderCurElem True (e, _) =
        withAttr "selected" $ txt e
    renderCurElem _ (e, _) = txt e
    renderContent ls =
        case listSelectedElement ls of
            Nothing -> txt "No Content"
            Just (_, (_, msg)) -> txt msg
