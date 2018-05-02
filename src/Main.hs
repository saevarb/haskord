{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
module Main where

import           Control.Concurrent
import           Control.Exception           (throwIO)
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Lazy             as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy              as TL (unpack)
import           GHC.Generics

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml                   as Y
import           Network.HTTP.Req
import           Network.HTTP.Req
import           Network.WebSockets          (ClientApp, Connection,
                                              ConnectionException (..),
                                              WebSocketsData (..),
                                              receiveData, sendClose,
                                              sendTextData)
import           Text.Pretty.Simple
import           Wuss
import Streaming
import qualified Streaming.Prelude as S
import Control.Category ((>>>))

import           Config
import           Http
import           Types



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
    -- conn <- gets wsConnection
    toGateway $ IdentifyCmd $ identPayload token
    startHeartbeatThread heartbeatInterval
-- dispatch conn (d -> Just (MessageCreateEvent (Message {..}))) = do

--     let embed =
--             embedTitle "This is an embed"
--             <> embedDesc "This is its description"
--             <> embedField "One" "Two"
--             <> embedIField "Inline" "Field"
--     when ("Hi bot" `T.isInfixOf` content) $ do
--         sendMessage channelId $
--            msgText "Hej" <>
--            msgEmbed embed <>
--            msgText "med dig"
-- dispatch _ p = do
--     liftIO $ pPrintNoColor p
--     return ()


updateSeqNo :: Maybe Int -> BotM ()
updateSeqNo Nothing = return ()
updateSeqNo (Just s) = do
    var <- gets seqNoVar
    liftIO $ atomically $ do
        empty <- isEmptyTMVar var
        if empty then
            putTMVar var s
            else
            void $ swapTMVar var s
    return ()


toGateway :: GatewayCommand -> BotM ()
toGateway x = do
    q <- gets gwQueue
    liftIO . atomically $ writeTQueue q x


wsSource :: Connection -> Stream (Of B.ByteString) IO ()
wsSource conn =
       S.repeatM (liftIO $ receiveData conn)

wsSink :: WebSocketsData a => Connection -> Stream (Of a) IO r -> IO r
wsSink conn = S.mapM_ (sendTextData conn)

parseCommand :: B.ByteString -> Either String RawGatewayCommand
parseCommand = eitherDecode

reportRawParseErrors
  :: Stream (Of String) (Stream (Of RawGatewayCommand) IO) r
     -> Stream (Of RawGatewayCommand) IO r
reportRawParseErrors =
    S.print

  -- :: Stream (Of RawGatewayCommand) IO r -> IO r
processGatewayCommands
  :: Stream (Of RawGatewayCommand) IO r
     -> Stream (Of (Maybe (Either String GatewayCommand))) IO r
processGatewayCommands =
    S.map rawToCommand

-- runRawPlugins =
--     S.store (S.mapM_ rawDispatch)

queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)

queueSink :: TQueue a -> Stream (Of a) IO r -> IO r
queueSink q stream =
    S.mapM_ (liftIO . atomically . writeTQueue q) stream

startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO r
startWriterThread gwq conn = do
    wsSink conn $ queueSource gwq

app :: BotConfig -> Connection -> IO ()
app cfg conn = do
    putStrLn "Connected!"
    writeFile "log" ""
    seqVar <- newEmptyTMVarIO
    sessionVar <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    startWriterThread gatewayQueue conn
    let botState = BotState sessionVar seqVar cfg gatewayQueue
    flip runStateT botState $ runBotM $ do
        liftIO
            $ S.print
            $ processGatewayCommands $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map parseCommand
            $ wsSource conn
        return ()
        -- let decoded = eitherDecode message :: Either String (RawGatewayCommand DispatchPayload)
        -- case decoded of
        --     Left err -> do
        --         let decoded' = decode message :: Maybe (RawGatewayCommand Value)
        --         updateSeqNo (s $ fromJust decoded')
        --         liftIO $ do
        --             pPrintNoColor decoded'
        --             putStrLn "=="
        --             appendFile "log" $ TL.unpack $ pShowNoColor decoded'
        --             appendFile "log" err
        --             putStrLn err
        --             putStrLn "==============\n"
        --     Right payload -> do
        --         updateSeqNo (s payload)
        --         liftIO $ pPrintNoColor payload
        --         dispatch conn payload
    return ()

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
