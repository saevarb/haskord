{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Control.Exception (throwIO)
import GHC.Generics
import Data.Text (Text, unpack, pack)
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.ByteString (ByteString)
import Network.WebSockets
import Data.Aeson
import Network.HTTP.Req
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Wuss


import Types

botToken :: ByteString
botToken = "<REDACTED>"
-- apiEndpoint :: [Char]
apiEndpoint :: ByteString
apiEndpoint = "https://discordapp.com/api/v6"


instance MonadHttp IO where
    handleHttpException = throwIO

dispatch :: MVar Int -> Connection -> GatewayMessage Payload -> IO ()
dispatch seqVar conn (d -> Just (HeartbeatPayload {..})) = do
    void $ forkIO $ do
        threadDelay (heartbeatInterval * 100)
        seqNo <- tryReadMVar seqVar
        sendTextData conn (mkHeartbeat seqNo)
dispatch _     _ _ = do
    return ()

updateSeqNo :: MVar Int -> (Maybe Int) -> IO ()
updateSeqNo _ Nothing = return ()
updateSeqNo var (Just s) = do
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
    let (Just (parsedUrl, _)) = parseUrlHttps apiEndpoint
        opt = header "Authorization" ("Bot " <> botToken) <>
              header "User-Agent" ("DiscordBot (https://github.com/saevarb/haskord, 0.1)")
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opt
    let gwResponse = responseBody res :: GatewayResponse
    print gwResponse
    runSecureClient (drop 6 . unpack $ url gwResponse) 443 "/?v=6&&encoding=json" app










