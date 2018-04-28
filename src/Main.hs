{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where

import           Control.Concurrent
import           Control.Exception           (throwIO)
import           Control.Monad
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml                   as Y
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


dispatch :: Text -> TVar (Maybe Int) -> Connection -> GatewayMessage Payload -> IO ()
dispatch token seqVar conn (d -> Just HeartbeatPayload {..}) = do
    void $ forkIO $ do
        threadDelay (heartbeatInterval * 1000)
        seqNo <- readTVarIO seqVar
        putStrLn "Sending heartbeat.."
        sendTextData conn (mkHeartbeat seqNo)
    sendTextData conn $ encode $ GatewayMessage { op = Identify, d = Just $ identPayload token, s = Nothing, t = Nothing}
dispatch _ _     _ _ =
    return ()

updateSeqNo :: TVar (Maybe Int) -> Maybe Int -> IO ()
updateSeqNo _ Nothing = return ()
updateSeqNo var (Just s) = do
    atomically $ writeTVar var (Just s)
    return ()

app :: BotConfig -> Connection -> IO b
app cfg conn = do
    putStrLn "Connected!"
    writeFile "log" ""
    seqVar <- newTVarIO Nothing
    forever $ do
        message <- receiveData conn
        let decoded = eitherDecode message :: Either String (GatewayMessage Payload)
        case decoded of
            Left err -> do
                pPrintNoColor $ (decode message :: Maybe (GatewayMessage Value))
                putStrLn "=="
                appendFile "log" err
                putStrLn err
                putStrLn "==============\n"
            Right payload -> do
                updateSeqNo seqVar (s payload)
                pPrintNoColor payload
                dispatch (botToken cfg) seqVar conn payload


handleException :: ConnectionException -> IO ()
handleException e = do
    putStrLn "Oops!"
    pPrint e

main :: IO ()
main = do
    cfg <- readConfig "config.yaml"
    case cfg of
        Left ex -> do
            putStrLn "Error reading config:"
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ url gateway) 443 "/?v=6&&encoding=json" (app cfg) `catch` handleException

















