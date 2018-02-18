{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception       (throwIO)
import           Control.Monad
import           Data.Text               (Text, pack, unpack)
import           Data.Text.Encoding      (decodeUtf8)
import           GHC.Generics

import           Data.Aeson
import qualified Data.Yaml               as Y
import           Network.HTTP.Req
import           Network.WebSockets      (ClientApp, Connection, receiveData,
                                          sendClose, sendTextData)
import           Wuss

import           Config
import           Http
import           Types

identPayload =
    IdentifyPayload
    { token          = "" -- decodeUtf8 botToken
    , properties     = IdentifyProperties "linux" "disco" "disco"
    , compress       = Nothing
    , largeThreshold = Nothing
    , shard          = Nothing
    , presence       = Nothing
    }


dispatch :: MVar Int -> Connection -> GatewayMessage Payload -> IO ()
dispatch seqVar conn (d -> Just HeartbeatPayload {..}) = do
    void $ forkIO $ do
        threadDelay (heartbeatInterval * 1000)
        seqNo <- tryReadMVar seqVar
        sendTextData conn (mkHeartbeat seqNo)
    sendTextData conn $ encode $ GatewayMessage { op = Identify, d = Just identPayload, s = Nothing, t = Nothing}
dispatch _     _ _ =
    return ()

updateSeqNo :: MVar Int -> Maybe Int -> IO ()
updateSeqNo _ Nothing = return ()
updateSeqNo var (Just s) =
    putMVar var s

app :: Connection -> IO b
app conn = do
    putStrLn "Connected!"
    seqVar <- newEmptyMVar
    forever $ do
        message <- receiveData conn
        let decoded = eitherDecode message :: Either String (GatewayMessage Payload)
        print message
        case decoded of
            Left err ->
                putStrLn err
            Right payload -> do
                updateSeqNo seqVar (s payload)
                print payload
                dispatch seqVar conn payload

main :: IO ()
main = do
    cfg <- readConfig "config.yaml"
    case cfg of
        Left ex -> do
            putStrLn "Error reading config:"
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ url gateway) 443 "/?v=6&&encoding=json" app











