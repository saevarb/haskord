{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throwIO)
import GHC.Generics
import Data.Text (Text, unpack, pack)
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Network.WebSockets
import Data.Aeson
import Network.HTTP.Req
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Wuss



botToken = "<REDACTED>"
-- apiEndpoint :: [Char]
apiEndpoint = "https://discordapp.com/api/v6"


data GatewayPayload a
    = GatewayPayload
    { op :: Int
    , d :: Maybe a
    , s :: Maybe Int
    , t :: Maybe Text
    } deriving (Generic, Show)

instance (ToJSON a) => ToJSON (GatewayPayload a)
instance (FromJSON a) => FromJSON (GatewayPayload a)


data DiscordPayload
    = Heartbeat
    { heartbeatInterval :: Int
    }
    -- | UnknownPayload
    deriving (Generic, Show)


decodingOptions :: Options
decodingOptions =
    defaultOptions
    { sumEncoding = UntaggedValue
    , fieldLabelModifier = camelTo2 '_'
    }

instance ToJSON DiscordPayload
instance FromJSON DiscordPayload where
    parseJSON = genericParseJSON decodingOptions


instance MonadHttp IO where
    handleHttpException = throwIO

data GatewayResponse
    = GatewayResponse
    { url :: Text
    , shards :: Maybe Int
    } deriving (Show, Generic)


instance ToJSON GatewayResponse
instance FromJSON GatewayResponse


mkHeartbeat :: (Maybe Int) -> _
mkHeartbeat v =
    encode $ object [ "op" .= (1 :: Int), "d" .= v ]

dispatch seqVar conn (Heartbeat {..}) = do
    void $ forkIO $ do
        threadDelay (heartbeatInterval * 1000)
        seq <- tryReadMVar seqVar
        sendTextData conn (mkHeartbeat seq)



updateSeqNo :: MVar Int -> (Maybe Int) -> IO ()
updateSeqNo _ Nothing = return ()
updateSeqNo var (Just s) = do
    putMVar var s

app conn = do
    putStrLn "Connected!"
    seqVar <- newEmptyMVar
    forever $ do
        message <- receiveData conn
        let decoded = eitherDecode message :: Either String (GatewayPayload DiscordPayload)
        print message
        case decoded of
            Left err ->
                putStrLn err
            Right payload -> do
                updateSeqNo seqVar (s payload)
                print payload
                dispatch seqVar conn (d payload)

main :: IO ()
main = do
    let (Just (parsedUrl, _)) = parseUrlHttps apiEndpoint
        opt = header "Authorization" ("Bot " <> botToken) <>
              header "User-Agent" ("DiscordBot (https://github.com/saevarb/haskord, 0.1)")
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opt
    let gwResponse = responseBody res :: GatewayResponse
    print gwResponse
    runSecureClient (drop 6 . unpack $ url gwResponse) 443 "/?v=6&&encoding=json" app










