{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Concurrent     hiding (throwTo)
import           Control.Exception      (throwIO)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.ByteString.Lazy   as B
import           Data.Monoid
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL (toStrict, unlines, unpack)
import qualified Data.Text.Lazy.IO      as T
import qualified Data.Vector            as V
import           GHC.Generics

import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml              as Y
import           Network.WebSockets     (ClientApp, Connection,
                                         ConnectionException (..),
                                         WebSocketsData (..), receiveData,
                                         sendClose, sendTextData)
import           Streaming              as S
import qualified Streaming.Prelude      as S
import           Text.Pretty.Simple
import           Wuss
import Brick.BChan

import           Config
import           Http
import           Types
import           Types.Common
import           Types.Gateway
import Rendering

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
                    _                      -> return ()
        _ -> return ()
    logI (TL.toStrict $ pShowNoColor et) (TL.toStrict $ pShowNoColor payload)
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
    when ("so sad" `T.isInfixOf` content) $ do
        sendMessage channelId $ msgText "\128546"
        -- sendMessage channelId $
        --   msgText "Hey" <>
        --   msgEmbed embed



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
  :: Stream (Of String) (Stream (Of RawGatewayCommand) BotM) r
  -> Stream (Of RawGatewayCommand) BotM r
reportRawParseErrors streams = do
    logger <- gets logErr
    S.mapM_ (\err -> liftIO $ logger "Raw parse error" $ pack err) streams
  where

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

logI :: Text -> Text -> BotM ()
logI title msg = do
    li <- gets logInfo
    liftIO $ li title msg
    -- ec <- gets eventChan
    -- liftIO $ writeBChan ec (title, msg)

logE :: Text -> Text -> BotM ()
logE title msg = do
    li <- gets logErr
    liftIO $ li title msg

app :: BotConfig -> Connection -> IO ()
app cfg conn = do
    -- putStrLn "Connected!"
    seqVar <- newEmptyTMVarIO
    sessionVar <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    errQ <- newTQueueIO
    logQ <- newTQueueIO
    eventChan <- newBChan 1000
    startWriterThread gatewayQueue conn
    let botState =
            BotState
            { sessionIdVar = sessionVar
            , seqNoVar     = seqVar
            , botConfig    = cfg
            , gwQueue      = gatewayQueue
            , logInfo      = makeLogger eventChan logQ MessageAdded
            , logErr       = makeLogger eventChan errQ ErrorAdded
            , eventChan    = eventChan
            }
    tid <- forkIO $ void $ flip runStateT botState $ runBotM $ do
            S.mapM_ rawDispatch . reportCommandParseErrors
            $ S.partitionEithers
            $ processGatewayCommands
            $ S.chain (updateSeqNo . s) $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map parseCommand
            $ wsSource conn
    renderInterface eventChan
    killThread tid
    return ()
  where
    makeLogger chan q event title msg = do
        liftIO $ writeBChan chan $ event (title, msg)
        -- liftIO $ atomically (writeTQueue q (title, msg))

handleException :: ConnectionException -> IO ()
handleException e = do
    putStrLn "Oops!"
    pPrint e


main :: IO ()
main = do
    cfg <- readConfig "config.yaml"
    case cfg of
        Left ex -> do
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ gwUrl gateway) 443 "/?v=6&&encoding=json" (app cfg) `catch` handleException
