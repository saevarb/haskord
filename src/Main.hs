{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throwIO)
import GHC.Generics
import Data.Text (Text)
import Data.Monoid

import Network.WebSockets
import Data.Aeson
import Network.HTTP.Req


botToken = "<REDACTED>"
-- apiEndpoint :: [Char]
apiEndpoint = "https://discordapp.com/api/v6"


instance MonadHttp IO where
    handleHttpException = throwIO

data GatewayResponse
    = GatewayResponse
    { url :: Text
    , shards :: Maybe Int
    } deriving (Show, Generic)


instance ToJSON GatewayResponse
instance FromJSON GatewayResponse


main :: IO ()
main = do
    let (Just (parsedUrl, _)) = parseUrlHttps apiEndpoint
        opt = header "Authorization" ("Bot " <> botToken)
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opt
    print (responseBody res :: GatewayResponse)







