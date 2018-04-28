{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Http where

import           Control.Exception  (throwIO)
import           Data.ByteString    (ByteString)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)
import Control.Monad.State

import           Network.HTTP.Req
import Data.Aeson (Value, encode)

import           Types
import Config

instance MonadHttp IO where
    handleHttpException = throwIO

apiEndpoint :: ByteString
apiEndpoint = "https://discordapp.com/api/v6"

Just (parsedUrl, _) = parseUrlHttps apiEndpoint

getGateway :: Text -> IO GatewayResponse
getGateway botToken = do
    let (Just (parsedUrl, _)) = parseUrlHttps apiEndpoint
        opt = header "Authorization" ("Bot " <> encodeUtf8 botToken) <>
              header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opt
    return $ responseBody res

sendMessage :: Snowflake -> Text -> BotM ()
sendMessage channel msg = do
    sendRequest POST (parsedUrl /: "channels" /~ channel /: "messages") (ReqBodyJson body) ignoreResponse
    return ()
  where
    body = defaultOutMessage { _content = Just msg }

sendRequest method path body resp = do
    tok <- gets (botToken . botConfig)
    let options =
            header "Authorization" ("Bot " <> encodeUtf8 tok)
            <> header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"
    res <- liftIO $ req method
        path
        body
        resp
        options
    return $ responseBody res
 where
