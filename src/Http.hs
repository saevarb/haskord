{-# LANGUAGE OverloadedStrings #-}
module Http where

import           Control.Exception  (throwIO)
import           Data.ByteString    (ByteString)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Network.HTTP.Req

import           Types

instance MonadHttp IO where
    handleHttpException = throwIO

apiEndpoint :: ByteString
apiEndpoint = "https://discordapp.com/api/v6"

getGateway :: Text -> IO GatewayResponse
getGateway botToken = do
    let (Just (parsedUrl, _)) = parseUrlHttps apiEndpoint
        opt = header "Authorization" ("Bot " <> encodeUtf8 botToken) <>
              header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opt
    return $ (responseBody res :: GatewayResponse)
