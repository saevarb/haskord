{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Types.User where

import Types.Common

data Presence
    = Presence
    { since  :: Integer
    , game   :: Maybe Activity
    , status :: Text
    , afk    :: Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON Presence where
    toJSON = genericToJSON decodingOptions
instance FromJSON Presence where
    parseJSON = genericParseJSON decodingOptions

data PartialPresenceUpdate
    = PartialPresenceUpdate
    { status :: Text
    , game   :: Maybe Activity
    , user   :: PartialUser
    }
    deriving (Show, Eq, Generic)

instance ToJSON PartialPresenceUpdate where
    toJSON = genericToJSON decodingOptions
instance FromJSON PartialPresenceUpdate where
    parseJSON = genericParseJSON decodingOptions


