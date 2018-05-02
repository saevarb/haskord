{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Concurrent
import           Control.Exception           (throwIO)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.ByteString.Lazy        as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy              as TL (unpack, unlines)
import qualified Data.Text.Lazy.IO           as T
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
                                              WebSocketsData (..), receiveData,
                                              sendClose, sendTextData)
import           Streaming
import qualified Streaming.Prelude           as S
import           Text.Pretty.Simple
import           Wuss

import           Config
import           Http
import Types
import Types.Gateway



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
rawDispatch _ = return ()
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
    putStrLn "Connected!"
    seqVar <- newEmptyTMVarIO
    sessionVar <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    startWriterThread gatewayQueue conn
    let botState = BotState sessionVar seqVar cfg gatewayQueue
    flip runStateT botState $ runBotM $ do
        Prelude.id
            $ S.mapM_ rawDispatch $ reportCommandParseErrors
            $ S.partitionEithers
            $ processGatewayCommands
            $ S.mapM (\x -> updateSeqNo (s x) >> return x) $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map parseCommand
            $ wsSource conn
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
