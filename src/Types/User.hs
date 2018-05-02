{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Types.User where

import Types.Common

data User
    = User
    { id_           :: Snowflake
    , username      :: Text
    , discriminator :: Text
    , avatar        :: Maybe Text
    , bot           :: Maybe Bool
    , mfaEnabled    :: Maybe Bool
    , verified      :: Maybe Bool
    , email         :: Maybe Text
    } deriving (Eq, Generic, Show)

instance ToJSON User where
    toJSON = genericToJSON decodingOptions
instance FromJSON User where
    parseJSON = genericParseJSON decodingOptions

data PartialUser
    = PartialUser
    { id_ :: Snowflake
    } deriving (Eq, Generic, Show)

instance ToJSON PartialUser where
    toJSON = genericToJSON decodingOptions
instance FromJSON PartialUser where
    parseJSON = genericParseJSON decodingOptions

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


