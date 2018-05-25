{-# LANGUAGE DuplicateRecordFields #-}
module Types.Guild where

import Types.Common
import Types.User

data GuildMember
    = GuildMember
    { user     :: User
    , nick     :: Maybe Text
    , roles    :: [Snowflake Role]
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
    { _id                         :: Snowflake Guild
    , name                        :: Text
    , icon                        :: Maybe Text
    , splash                      :: Maybe Text
    , owner                       :: Maybe Bool
    , ownerId                     :: Snowflake User
    , permissions                 :: Maybe Word64
    , region                      :: Text
    , afkChannelId                :: Maybe (Snowflake Channel)
    , afkTimeout                  :: Word64
    , embedEnabled                :: Maybe Bool
    , embedChannelId              :: Maybe (Snowflake Channel)
    , verificationLevel           :: Word64
    , defaultMessageNotifications :: Word64
    , explicitContentFilter       :: Word64
    , roles                       :: [Role]
    , emojis                      :: [Emoji]
    , features                    :: [Text]
    , mfaLevel                    :: Word64
    , applicationId               :: Maybe (Snowflake Application)
    , widgetEnabled               :: Maybe Bool
    , widgetChannelId             :: Maybe (Snowflake Channel)
    , systemChannelId             :: Maybe (Snowflake Channel)
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
    { _id          :: Snowflake Guild
    , unavailable :: Bool
    } deriving (Generic, Eq, Show)

instance ToJSON UnavailableGuild where
    toJSON = genericToJSON decodingOptions
instance FromJSON UnavailableGuild where
    parseJSON = genericParseJSON decodingOptions
