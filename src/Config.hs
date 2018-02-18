{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Text    (Text)
import           GHC.Generics

import           Data.Yaml

data BotConfig
    = BotConfig
    { botToken :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON BotConfig

readConfig :: FilePath -> IO (Either ParseException BotConfig)
readConfig = decodeFileEither
