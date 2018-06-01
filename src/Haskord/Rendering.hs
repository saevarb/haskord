{-# LANGUAGE OverloadedStrings #-}
module Haskord.Rendering where

import Control.Monad.IO.Class
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL (toStrict, unpack, unlines)
import qualified Data.Vector            as V
import Data.Maybe
import Data.Bool
import Data.Char

import Graphics.Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.BChan
import           Text.Pretty.Simple
import Control.Concurrent.STM
import Control.Concurrent.Async
import           Streaming                     as S
import qualified Streaming.Prelude             as S

import Haskord.Logging

data Screen
    = Log
    | Chat
    deriving (Ord, Eq, Show)


data RenderEvent
    = Rerender
    deriving (Show, Eq, Ord)

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
renderInterface lv bchan = do
    -- async $ do
    --     S.mapM_ (writeBChan bchan)
    --     . S.delay (recip 10)
    --     $ S.repeat Rerender
    void $ customMain (mkVty defaultConfig) (Just bchan) brickApp (initialRenderingState lv)

renderChat :: RenderingState -> Widget Screen
renderChat _ =
    border $ vCenter $ hCenter $ str "This is the chat"

renderLog :: RenderingState -> Widget Screen
renderLog RenderingState {..} =
    border (renderList renderCurElem True logMessages)
    -- <+> (renderContent logMessages)
  where
    renderCurElem selected LogMessage {..} =
        let fn = bool id (withAttr "selected") selected
        in fn $ (withAttr (attrName $ sevToString severity) $ txt $ prefix severity) <+> txtWrap title
    prefix Info = " ++ "
    prefix Warning = " !! "
    prefix Error = " ***** "
    prefix Fatal = " -- FATAL ERROR -- "
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
  :: RenderingState
     -> BrickEvent n RenderEvent -> EventM Screen (Next RenderingState)
eventHandler s (VtyEvent (EvKey (KChar 'q') _)) = do
    halt s
eventHandler s (VtyEvent (EvKey (KChar '1') _)) = do
    continue s { screen = Log }
eventHandler s (VtyEvent (EvKey (KChar '2') _)) = do
    continue s { screen = Chat }
eventHandler s@RenderingState {..} (VtyEvent event) = do
    logMsgs <- liftIO $ atomically $ readTVar logV
    newList' <- handleListEventVi handleListEvent event logMessages
    let newList = listReplace (V.fromList $ toList logMsgs) (listSelected newList') newList'
    continue $ s { logMessages = newList }
-- eventHandler s@RenderingState {..} (AppEvent Rerender) = do
    -- logMsgs <- liftIO $ atomically $ readTVar logV
    -- let newList = listReplace (V.fromList $ toList logMsgs) (listSelected logMessages) logMessages
    -- continue $ s { logMessages = newList }
eventHandler s@RenderingState {..} _ = do
    -- logMsgs <- liftIO $ atomically $ readTVar logV
    -- let newList = listReplace (V.fromList $ toList logMsgs) (listSelected logMessages) logMessages
    -- continue $ s { logMessages = newList }
    continue s


renderAll :: RenderingState -> [Widget Screen]
renderAll s =
    let curTab = fromJust $ lookup (screen s) tabs
    in [renderTabs (screen s) <=> tabRenderer curTab s]


