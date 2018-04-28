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
import qualified Data.ByteString             as B
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
                                              receiveData, sendClose,
                                              sendTextData)
import           Text.Pretty.Simple
import           Wuss

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


startHeartbeatThread :: Int -> Connection -> BotM ()
startHeartbeatThread interval conn = do
    sv <- gets seqNoVar
    liftIO $ void $ forkIO $ do
        threadDelay (interval * 1000)
        seqNo <- atomically $ tryReadTMVar sv
        putStrLn $ "Sending heartbeat.." ++ show seqNo
        sendTextData conn (mkHeartbeat seqNo)

dispatch :: Connection -> GatewayMessage Payload -> BotM ()
dispatch conn (d -> Just HeartbeatPayload {..}) = do
    token <- gets (botToken . botConfig)
    liftIO $ sendTextData conn $ encode GatewayMessage { op = Identify, d = Just $ identPayload token, s = Nothing, t = Nothing}
    startHeartbeatThread heartbeatInterval conn
dispatch conn (d -> Just (MessagePayload (Message {..}))) = do

    let embed =
            embedTitle "This is an embed"
            <> embedDesc "This is its description"
            <> embedField "One" "Two"
            <> embedIField "Inline" "Field"
    when ("Hi bot" `T.isInfixOf` content) $ do
        sendMessage channelId $
           msgText "Hej" <>
           msgEmbed embed
dispatch _ _ =
    return ()


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

app :: BotState -> Connection -> IO ()
app botState conn = do
    putStrLn "Connected!"
    writeFile "log" ""
    flip runStateT botState $ runBotM $ forever $ do
        message <- liftIO $ receiveData conn
        let decoded = eitherDecode message :: Either String (GatewayMessage Payload)
        case decoded of
            Left err -> do
                let decoded' = decode message :: Maybe (GatewayMessage Value)
                updateSeqNo (s $ fromJust decoded')
                liftIO $ do
                    pPrintNoColor decoded'
                    putStrLn "=="
                    appendFile "log" $ TL.unpack $ pShowNoColor decoded'
                    appendFile "log" err
                    putStrLn err
                    putStrLn "==============\n"
            Right payload -> do
                updateSeqNo (s payload)
                liftIO $ pPrintNoColor payload
                dispatch conn payload
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
            seqVar <- newEmptyTMVarIO
            sessionVar <- newEmptyTMVarIO
            let botState = BotState sessionVar seqVar cfg
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ gwUrl gateway) 443 "/?v=6&&encoding=json" (app botState) `catch` handleException

















