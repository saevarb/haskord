{-# LANGUAGE DuplicateRecordFields #-}
module Types.Guild where

import Types.Common
import Types.User
import Types.Channel

data GuildMember
    = GuildMember
    { user     :: User
    , nick     :: Maybe Text
    , roles    :: [Snowflake]
    , joinedAt :: Maybe Timestamp
    , deaf     :: Bool
    , mute     :: Bool
    } deriving (Eq, Show, Generic)


instance ToJSON GuildMember where
    toJSON = genericToJSON decodingOptions
instance FromJSON GuildMember where
    parseJSON = genericParseJSON decodingOptions


data Guild
    = Guild
    { _id                         :: Snowflake
    , name                        :: Text
    , icon                        :: Maybe Text
    , splash                      :: Maybe Text
    , owner                       :: Maybe Bool
    , ownerId                     :: Snowflake
    , permissions                 :: Maybe Word64
    , region                      :: Text
    , afkChannelId                :: Maybe Snowflake
    , afkTimeout                  :: Word64
    , embedEnabled                :: Maybe Bool
    , embedChannelId              :: Maybe Snowflake
    , verificationLevel           :: Word64
    , defaultMessageNotifications :: Word64
    , explicitContentFilter       :: Word64
    , roles                       :: [Role]
    , emojis                      :: [Emoji]
    , features                    :: [Text]
    , mfaLevel                    :: Word64
    , applicationId               :: Maybe Snowflake
    , widgetEnabled               :: Maybe Bool
    , widgetChannelId             :: Maybe Snowflake
    , systemChannelId             :: Maybe Snowflake
    , joinedAt                    :: Maybe Timestamp
    , large                       :: Maybe Bool
    , unavailable                 :: Maybe Bool
    , memberCount                 :: Maybe Int
    , voiceStates                 :: Maybe [VoiceState]
    , members                     :: Maybe [GuildMember]
    , channels                    :: Maybe [Channel]
    , presences                   :: Maybe [PartialPresenceUpdate]
    } deriving (Eq, Generic, Show)

instance ToJSON Guild where
    toJSON = genericToJSON decodingOptions
instance FromJSON Guild where
    parseJSON = genericParseJSON decodingOptions

data UnavailableGuild
    = UnavailableGuild
    { _id          :: Snowflake
    , unavailable :: Bool
    } deriving (Generic, Eq, Show)

instance ToJSON UnavailableGuild where
    toJSON = genericToJSON decodingOptions
instance FromJSON UnavailableGuild where
    parseJSON = genericParseJSON decodingOptions
