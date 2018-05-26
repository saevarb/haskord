{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Concurrent hiding (throwTo)
import           Control.Exception           (throwIO)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.ByteString.Lazy        as B
import           Data.Monoid
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy              as TL (toStrict, unpack, unlines)
import qualified Data.Text.Lazy.IO           as T
import           GHC.Generics
import qualified Data.Vector as V

import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml                   as Y
import           Network.WebSockets          (ClientApp, Connection,
                                              ConnectionException (..),
                                              WebSocketsData (..), receiveData,
                                              sendClose, sendTextData)
import           Streaming
import qualified Streaming.Prelude           as S
import           Text.Pretty.Simple
import           Wuss
import Graphics.Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border

import           Config
import           Http
import Types
import Types.Gateway
import Types.Common

identPayload :: Text -> IdentifyPayload
identPayload token =
    IdentifyPayload
    { token          = token
    , properties     = IdentifyProperties "linux" "disco" "disco"
    , compress       = Nothing
    , largeThreshold = Nothing
    , shard          = Nothing
    , presence       = Nothing
    }


startHeartbeatThread :: Int -> BotM ()
startHeartbeatThread interval = do
    -- sv <- gets seqNoVar
    gwq <- gets gwQueue
    liftIO $ void $ forkIO $ do
        queueSink gwq
        $ S.delay (fromIntegral interval / 1000)
        $ S.repeat HeartbeatCmd

rawDispatch :: GatewayCommand -> BotM ()
rawDispatch (HelloCmd (Heartbeat' {..})) = do
    token <- gets (botToken . botConfig)
    toGateway $ IdentifyCmd $ identPayload token
    startHeartbeatThread heartbeatInterval
-- rawDispatch (DispatchCmd MESSAGE_CREATE _ payload) = do
rawDispatch (DispatchCmd et _ payload) = do
    case et of
        MESSAGE_CREATE ->
                case payload of
                    MessageCreateEvent msg -> helloPlugin msg
                    _ -> return ()
        _ -> return ()
    -- liftIO $ pPrint et
    -- liftIO $ pPrint payload
rawDispatch _ = return ()


helloPlugin :: Message -> BotM ()
helloPlugin = \(Message {..}) -> do
    let embed =
            embedTitle "This is an embed"
            <> embedDesc "This is its description"
            <> embedField "One" "Two"
            <> embedIField "Inline" "Field"
    when ("Hi bot" `T.isInfixOf` content) $ do
        sendMessage channelId $
          msgText "Hey" <>
          msgEmbed embed



updateSeqNo :: Maybe Int -> BotM ()
updateSeqNo Nothing = return ()
updateSeqNo (Just s) = do
    var <- gets seqNoVar
    liftIO $ atomically $
        isEmptyTMVar var >>= \case
            True -> putTMVar var s
            _ -> void $ swapTMVar var s
    return ()


toGateway :: GatewayCommand -> BotM ()
toGateway x = do
    q <- gets gwQueue
    liftIO . atomically $ writeTQueue q x


wsSource :: MonadIO m => Connection -> Stream (Of B.ByteString) m ()
wsSource conn =
       S.repeatM (liftIO $ receiveData conn)

wsSink :: WebSocketsData a => Connection -> Stream (Of a) IO r -> IO r
wsSink conn = S.mapM_ (sendTextData conn)

parseCommand :: B.ByteString -> Either String RawGatewayCommand
parseCommand = eitherDecode

reportRawParseErrors
  :: MonadIO m
  => Stream (Of String) (Stream (Of RawGatewayCommand) m) r
  -> Stream (Of RawGatewayCommand) m r
reportRawParseErrors =
    S.print

reportCommandParseErrors
    :: MonadIO m
    => Stream (Of (RawGatewayCommand, String)) (Stream (Of GatewayCommand) m) r
    -> Stream (Of GatewayCommand) m r
reportCommandParseErrors =
    S.mapM_ $ \(msg, err) -> liftIO $ do
        T.appendFile "error.log" $
        -- T.putStrLn $
             TL.unlines
             [ "Parse error: "
             , pShowNoColor err
             , pShowNoColor msg
             ]
processGatewayCommands
  :: Stream (Of RawGatewayCommand) BotM r
  -> Stream (Of (Either (RawGatewayCommand, String) GatewayCommand)) BotM r
processGatewayCommands =
    S.map $ \x -> first (x,) $ rawToCommand x

queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)

queueSink :: TQueue a -> Stream (Of a) IO r -> IO r
queueSink q stream =
    S.mapM_ (liftIO . atomically . writeTQueue q) stream

startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO ()
startWriterThread gwq conn = do
    void $ forkIO $ wsSink conn $ queueSource gwq

app :: BotConfig -> Connection -> IO ()
app cfg conn = do
    -- putStrLn "Connected!"
    seqVar <- newEmptyTMVarIO
    sessionVar <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    errList <- newTMVarIO V.empty
    logList <- newTMVarIO V.empty
    startWriterThread gatewayQueue conn
    let botState =
            BotState
            { sessionIdVar = sessionVar
            , seqNoVar = seqVar
            , botConfig = cfg
            , gwQueue = gatewayQueue
            , errMsgs = errList
            , logMsgs = logList
            }
    let renderingState =
            RenderingState
            { errMessages = list ErrList V.empty 10
            -- , logMessages = V.empty
            -- , errMessages = V.empty
            , logMessages = list LogList V.empty 10
            , screen = LogList
            }
    tid <- forkIO $ void $ flip runStateT botState $ runBotM $ do
            S.mapM_ rawDispatch . reportCommandParseErrors
            $ S.partitionEithers
            $ processGatewayCommands
            $ S.mapM (\x -> updateSeqNo (s x) >> return x) $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map parseCommand
            $ wsSource conn
    defaultMain brickApp renderingState
    killThread tid
    return ()

handleException :: ConnectionException -> IO ()
handleException e = do
    putStrLn "Oops!"
    pPrint e


data Screen
    = LogList
    | ErrList
    | Chat
    deriving (Ord, Eq, Show)

data RenderingState
    = RenderingState
    { logMessages :: List Screen (Text, Text)
    , errMessages :: List Screen (Text, Text)
    , screen      :: Screen
    }

brickApp :: App RenderingState e Screen
brickApp =
    App
    { appDraw = render
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = eventHandler
    , appStartEvent = startEvent
    , appAttrMap = myAttrMap
    }
  where
    startEvent s = return s
    myAttrMap _ = attrMap Graphics.Vty.defAttr [("selected", bg white)]
    eventHandler s (VtyEvent (EvKey (KChar 'q') _)) = do
        halt s
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
            Just (_, (_, msg)) -> txt . TL.toStrict $ pShow msg

main :: IO ()
main = do
    cfg <- readConfig "config.yaml"
    case cfg of
        Left ex -> do
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ gwUrl gateway) 443 "/?v=6&&encoding=json" (app cfg) `catch` handleException
