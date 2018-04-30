{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Types where

import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.Text            (Text, pack, unpack)
import           Data.Word            (Word64)
import           GHC.Generics
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Control.Applicative

import           Data.Aeson
import           Data.Scientific
import           Web.HttpApiData
import           Control.Concurrent.STM

import Config

data BotState
    = BotState
    { sessionIdVar :: TMVar String
    , seqNoVar     :: TMVar Int
    , botConfig    :: BotConfig
    }

newtype BotM a
    = BotM
    { runBotM :: StateT BotState IO a
    } deriving (Applicative, Monad, MonadIO, MonadState BotState, Functor)


data GatewayOpcode
    = Dispatch
    | Heartbeat
    | Identify
    | StatusUpdate
    | VoiceStateUpdate
    | VoiceServerPing
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatACK
    | UnknownError
    | UnknownOpcode
    deriving (Show, Eq, Enum)


opcodeMap :: [(Int, GatewayOpcode)]
opcodeMap =
    zip ([0 .. 11]) [toEnum 0 ..]

reverseOpcodeMap :: [(GatewayOpcode, Int)]
reverseOpcodeMap =
    map swap opcodeMap
  where
    swap (x, y) = (y, x)

instance FromJSON GatewayOpcode where
    parseJSON =
        withScientific "opcode" $ \n ->
            case floatingOrInteger n of
                Left _ ->
                    fail "Opcode was not an integer"
                Right i ->
                    maybe (fail "Unknown opcode") return $ lookup i opcodeMap

instance ToJSON GatewayOpcode where
    toJSON opcode =
        toJSON $ fromJust (lookup opcode reverseOpcodeMap)

decodingOptions :: Options
decodingOptions =
    defaultOptions
    { sumEncoding = UntaggedValue
    , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
    , omitNothingFields = True
    }

data EventType
    = HELLO
    | READY
    | RESUMED
    | INVALID_SESSION
    | CHANNEL_CREATE
    | CHANNEL_UPDATE
    | CHANNEL_DELETE
    | CHANNEL_PINS_UPDATE
    | GUILD_CREATE
    | GUILD_UPDATE
    | GUILD_DELETE
    | GUILD_BAN_ADD
    | GUILD_BAN_REMOVE
    | GUILD_EMOJIS_UPDATE
    | GUILD_INTEGRATIONS_UPDATE
    | GUILD_MEMBER_ADD
    | GUILD_MEMBER_REMOVE
    | GUILD_MEMBER_UPDATE
    | GUILD_MEMBERS_CHUNK
    | GUILD_ROLE_CREATE
    | GUILD_ROLE_UPDATE
    | GUILD_ROLE_DELETE
    | MESSAGE_CREATE
    | MESSAGE_UPDATE
    | MESSAGE_DELETE
    | MESSAGE_DELETE_BULK
    | MESSAGE_REACTION_ADD
    | MESSAGE_REACTION_REMOVE
    | MESSAGE_REACTION_REMOVE_ALL
    | PRESENCE_UPDATE
    | TYPING_START
    | USER_UPDATE
    | VOICE_STATE_UPDATE
    | VOICE_SERVER_UPDATE
    | WEBHOOKS_UPDATE
    deriving (Show, Eq, Enum, Generic)

instance FromJSON EventType
instance ToJSON EventType


data GatewayMessage payloadType
    = GatewayMessage
    { op :: GatewayOpcode
    , d  :: Maybe payloadType
    , s  :: Maybe Int
    , t  :: Maybe EventType
    } deriving (Generic, Show)

instance (ToJSON a) => ToJSON (GatewayMessage a)
instance (FromJSON a) => FromJSON (GatewayMessage a)

data IdentifyProperties
    = IdentifyProperties
    { os      :: Text
    , browser :: Text
    , device  :: Text
    }
    deriving (Show, Eq, Generic)



instance ToJSON IdentifyProperties where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = ('$' :) }
instance FromJSON IdentifyProperties where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = ('$' :) }

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

newtype Snowflake = Snowflake Word64
    deriving (Show, Eq, Generic)

instance ToJSON Snowflake
instance FromJSON Snowflake where
    parseJSON = withText "Snowflake" $ \s -> do
        return $ Snowflake (read $ unpack s)

instance ToHttpApiData Snowflake where
    toUrlPiece (Snowflake x) = pack $ show x

data OutMessage
    = OutMessage
    { _content    :: Maybe Text
    , _tts         :: Bool
    , file        :: Maybe Text
    , embed       :: Maybe Embed
    , payloadJson :: Maybe Text
    } deriving (Show, Eq, Generic)


instance Monoid OutMessage where
    mappend m1 m2 =
        joinMessages m1 m2
    mempty = defaultOutMessage

msgText :: Text -> OutMessage
msgText t = mempty { _content = Just t }

msgEmbed :: Embed -> OutMessage
msgEmbed e = mempty { embed = Just e }


joinMessages :: OutMessage -> OutMessage -> OutMessage
joinMessages m1 m2 =
    OutMessage
    { _content = _content m1 <> _content m2
    , _tts      = _tts m1 || _tts m2
    , file     = file m1 <> file m2
    , embed    = embed m1 <> embed m2
    , payloadJson = payloadJson m1 <> payloadJson m2
    }
  where
    isEmbed = isJust (embed m1) || isJust (embed m2)

defaultOutMessage =
    OutMessage Nothing False Nothing Nothing Nothing

instance ToJSON OutMessage where
    toJSON = genericToJSON decodingOptions
instance FromJSON OutMessage where
    parseJSON = genericParseJSON decodingOptions


data Message
    = Message
    { id_             :: Snowflake
    , channelId       :: Snowflake
    , author          :: User
    , content         :: Text
    , timestamp       :: Timestamp
    , editedTimestamp :: Maybe Text
    , tts             :: Bool
    , mentionEveryone :: Bool
    , mentions        :: [Mention]
    , mentionRoles    :: [Role]
    , embeds          :: [Embed]
    , reactions       :: Maybe [Reaction]
    , nonce           :: Maybe Snowflake
    , pinned          :: Bool
    , webhookId       :: Maybe Snowflake
    , type_           :: Int
    , activity        :: Maybe Activity
    , application     :: Maybe Application
    } deriving (Show, Eq, Generic)

instance ToJSON Message where
    toJSON = genericToJSON decodingOptions
instance FromJSON Message where
    parseJSON = genericParseJSON decodingOptions

-- data Activity
--     = Activity
--     deriving (Show, Eq, Generic)

-- instance ToJSON Activity
-- instance FromJSON Activity

type Mention = Value
type Application = Value
type Channel = Value
type Emoji = Value
type VoiceState = Value
type Timestamp = Value
type Permissions = Word64

data Role
    = Role
    { _id         :: Snowflake
    , name        :: Text
    , _color       :: Word64
    , hoist       :: Bool
    , position    :: Word64
    , permissions :: Permissions
    , managed     :: Bool
    , mentionable :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON Role where
    toJSON = genericToJSON decodingOptions
instance FromJSON Role where
    parseJSON = genericParseJSON decodingOptions


data GuildMember
    = GuildMember
    { user :: User
    , nick :: Maybe Text
    , roles :: [Snowflake]
    , joinedAt :: Maybe Timestamp
    , deaf :: Bool
    , mute :: Bool
    } deriving (Eq, Show, Generic)


instance ToJSON GuildMember where
    toJSON = genericToJSON decodingOptions
instance FromJSON GuildMember where
    parseJSON = genericParseJSON decodingOptions

data Embed
    = Embed
    { title       :: Maybe Text
    , _type       :: Maybe Text
    , description :: Maybe Text
    , _url        :: Maybe Text
    , _timestamp  :: Maybe Timestamp
    , color       :: Maybe Word64
    , footer      :: Maybe EmbedFooter
    , image       :: Maybe EmbedImage
    , thumbnail   :: Maybe EmbedThumbnail
    , video       :: Maybe EmbedVideo
    , provider    :: Maybe EmbedProvider
    , _author     :: Maybe EmbedAuthor
    , fields      :: Maybe [EmbedField]
    } deriving (Eq, Show, Generic)

instance ToJSON Embed where
    toJSON = genericToJSON decodingOptions
instance FromJSON Embed where
    parseJSON = genericParseJSON decodingOptions

instance Monoid Embed where
    mappend e1 e2 = joinEmbeds e1 e2
    mempty =
        Embed
        { title       = Nothing
        , _type       = Nothing
        , description = Nothing
        , _url        = Nothing
        , _timestamp  = Nothing
        , color       = Nothing
        , footer      = Nothing
        , image       = Nothing
        , thumbnail   = Nothing
        , video       = Nothing
        , provider    = Nothing
        , _author      = Nothing
        , fields      = Nothing
        }

joinEmbeds :: Embed -> Embed -> Embed
joinEmbeds e1 e2 =
    Embed
    { title       = title e1 <> title e2
    , _type       = prefLatter _type e1 e2
    , description = description e1 <> description e2
    , _url        = prefLatter _url e1 e2
    , _timestamp  = prefLatter _timestamp e1 e2
    , color       = prefLatter color e1 e2
    , footer      = prefLatter footer e1 e2
    , image       = prefLatter image e1 e2
    , thumbnail   = prefLatter thumbnail e1 e2
    , video       = prefLatter video e1 e2
    , provider    = prefLatter provider e1 e2
    , _author     = prefLatter _author e1 e2
    , fields      = fields e1 <> fields e2
    }
  where
    prefLatter f m1 m2 = f m2 <|> f m1

embedTitle t = mempty { title = Just t }
embedDesc d = mempty { description = Just d }
embedField k v = mempty { fields = Just [EmbedField k v Nothing]}
embedIField k v = mempty { fields = Just [EmbedField k v (Just True)]}

data EmbedFooter
    = EmbedFooter
    { _text   :: Text
    , iconUrl :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedFooter where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedFooter where
    parseJSON = genericParseJSON decodingOptions

--  w"kyiw0}"apeewve"kp0jjeewve"kp0jj

data EmbedThumbnail
    = EmbedThumbnail
    { url    :: Maybe Text
    , height :: Maybe Int
    , width  :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedThumbnail where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedThumbnail where
    parseJSON = genericParseJSON decodingOptions

data EmbedVideo
    = EmbedVideo
    { url    :: Maybe Text
    , height :: Maybe Int
    , width  :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedVideo where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedVideo where
    parseJSON = genericParseJSON decodingOptions

data EmbedImage
    = EmbedImage
    { url    :: Maybe Text
    , height :: Maybe Int
    , width  :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedImage where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedImage where
    parseJSON = genericParseJSON decodingOptions

data EmbedProvider
    = EmbedProvider
    { name :: Maybe Text
    , url  :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedProvider where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedProvider where
    parseJSON = genericParseJSON decodingOptions

data EmbedAuthor
    = EmbedAuthor
    { name    :: Maybe Text
    , url     :: Maybe Text
    , iconUrl :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedAuthor where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedAuthor where
    parseJSON = genericParseJSON decodingOptions

data EmbedField
    = EmbedField
    { name :: String
    , value :: String
    , inline :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedField where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedField where
    parseJSON = genericParseJSON decodingOptions

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
    } deriving (Generic, Show)

instance ToJSON Guild where
    toJSON = genericToJSON decodingOptions
instance FromJSON Guild where
    parseJSON = genericParseJSON decodingOptions

data Ready
    = Ready
    { v               :: Word64
    , user            :: User
    , privateChannels :: [Channel]
    , guilds          :: [UnavailableGuild]
    , sessionId       :: Text
    } deriving (Generic, Show)

instance ToJSON Ready where
    toJSON = genericToJSON decodingOptions
instance FromJSON Ready where
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

data TypingStart
    = TypingStart
    { channelId :: Snowflake
    , userId    :: Snowflake
    , timestamp :: Timestamp
    , guildId   :: Snowflake
    } deriving (Generic, Eq, Show)

instance ToJSON TypingStart where
    toJSON = genericToJSON decodingOptions
instance FromJSON TypingStart where
    parseJSON = genericParseJSON decodingOptions

data PresenceUpdate
    = PresenceUpdate
    { user    :: PartialUser
    , roles   :: [Snowflake]
    , game    :: Maybe Activity
    , guildId :: Snowflake
    , status  :: Text
    } deriving (Generic, Eq, Show)

instance ToJSON PresenceUpdate where
    toJSON = genericToJSON decodingOptions
instance FromJSON PresenceUpdate where
    parseJSON = genericParseJSON decodingOptions

data Activity
    = Activity
    { name :: Text
    , type_ :: Word64
    , url :: Maybe Text
    , timestamps :: Maybe Timestamps
    , applicationId :: Maybe Snowflake
    , details :: Maybe Text
    , state :: Maybe Text
    , party :: Maybe Party
    , assets :: Maybe Assets
    , _instance :: Maybe Bool
    , flags :: Maybe Int
    } deriving (Generic, Eq, Show)

instance ToJSON Activity where
    toJSON = genericToJSON decodingOptions
instance FromJSON Activity where
    parseJSON = genericParseJSON decodingOptions
type Assets = Value
type Timestamps = Value
type Party = Value

data Reaction
    = Reaction
    { userId :: Snowflake
    , channelId :: Snowflake
    , reaction :: Snowflake
    , emoji :: PartialEmoji
    } deriving (Generic, Eq, Show)

type PartialEmoji = Value

instance ToJSON Reaction where
    toJSON = genericToJSON decodingOptions
instance FromJSON Reaction where
    parseJSON = genericParseJSON decodingOptions


data Payload
    = HeartbeatPayload
    { heartbeatInterval :: Int
    }
    | ReadyPayload Ready
    | IdentifyPayload
    { token          :: Text
    , properties     :: IdentifyProperties
    , compress       :: Maybe Bool
    , largeThreshold :: Maybe Int
    , shard          :: Maybe (Int, Int)
    , presence       :: Maybe Presence
    }
    | MessagePayload Message
    | GuildCreatePayload Guild
    | MessageReactionAdd Reaction
    | MessageReactionRemove Reaction
    | TypingStartPayload TypingStart
    | PresenceUpdatePayload PresenceUpdate
    -- | UnknownSoFar Value
    deriving (Generic, Show)

instance ToJSON Payload where
    toJSON = genericToJSON decodingOptions
instance FromJSON Payload where
    parseJSON = genericParseJSON decodingOptions

data GatewayResponse
    = GatewayResponse
    { gwUrl    :: Text
    , gwShards :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON GatewayResponse where
    toJSON = genericToJSON decodingOptions { fieldLabelModifier = camelTo2 '_' . drop 2}
instance FromJSON GatewayResponse where
    parseJSON = genericParseJSON decodingOptions { fieldLabelModifier = camelTo2 '_' . drop 2}

mkHeartbeat :: Maybe Int -> ByteString
mkHeartbeat v =
    encode $ GatewayMessage { d = v, op = Heartbeat, t = Nothing, s = Nothing}

