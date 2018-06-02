{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskord.Http where

import           Control.Exception   (throwIO)
import           Control.Monad.Reader
import           Data.ByteString     (ByteString)
import           Data.Monoid         ((<>))
import           Data.Proxy
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8)

import           Data.Aeson          (Value, encode)
import           Network.HTTP.Req

import           Haskord.Config
import           Haskord.Types
import           Haskord.Types.Channel
import           Haskord.Types.Common
import           Haskord.Types.Gateway

instance MonadHttp IO where
    handleHttpException = throwIO

apiEndpoint :: ByteString
apiEndpoint = "https://discordapp.com/api/v6"

parsedUrl :: Url 'Https
Just (parsedUrl, _) = parseUrlHttps apiEndpoint

getGateway :: Text -> IO GatewayResponse
getGateway tok = do
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opts
    return $ responseBody res
  where
    opts =
        mconcat
        [ header "Authorization" ("Bot " <> encodeUtf8 tok)
        , header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"
        ]

sendMessage :: Snowflake Channel -> OutMessage -> BotM ()
sendMessage channel msg = do
    sendRequest POST (parsedUrl /: "channels" /~ channel /: "messages") (ReqBodyJson msg) ignoreResponse
    return ()
  where

sendRequest
  :: (MonadReader BotState m, MonadIO m, HttpMethod method,
      HttpBody body, HttpResponse response,
      HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
     method
     -> Url scheme
     -> body
     -> Proxy response
     -> m (HttpResponseBody response)
sendRequest method path body resp = do
    tok <- asks (botToken . botConfig)
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
