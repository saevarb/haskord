{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text            (Text, pack, unpack)
import           Data.Word            (Word64)
import           GHC.Generics

import           Data.Aeson
import           Data.Scientific

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

newtype Snowflake = Snowflake Word64
    deriving (Show, Eq, Generic)

instance ToJSON Snowflake
instance FromJSON Snowflake where
    parseJSON = withText "Snowflake" $ \s -> do
        return $ Snowflake (read $ unpack s)


-- data Activity
--     = Activity
--     deriving (Show, Eq, Generic)

-- instance ToJSON Activity
-- instance FromJSON Activity

type User = Value
type Mention = Value
type Role = Value
type Embed = Value
type Reaction = Value
type Activity = Value
type Application = Value

data Payload
    = HeartbeatPayload
    { heartbeatInterval :: Int
    }
    | IdentifyPayload
    { token          :: Text
    , properties     :: IdentifyProperties
    , compress       :: Maybe Bool
    , largeThreshold :: Maybe Int
    , shard          :: Maybe (Int, Int)
    , presence       :: Maybe Presence
    }
    | MessagePayload
    { id              :: Snowflake
    , channelId       :: Snowflake
    , author          :: User
    , content         :: Text
    , timestamp       :: Text
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
    }
    -- | UnknownSoFar Value
    deriving (Generic, Show)

instance ToJSON Payload where
    toJSON = genericToJSON decodingOptions
instance FromJSON Payload where
    parseJSON = genericParseJSON decodingOptions

data GatewayResponse
    = GatewayResponse
    { url    :: Text
    , shards :: Maybe Int
    } deriving (Show, Generic)


instance ToJSON GatewayResponse
instance FromJSON GatewayResponse


mkHeartbeat :: Maybe Int -> ByteString
mkHeartbeat v =
    encode $ GatewayMessage { d = v, op = Heartbeat, t = Nothing, s = Nothing}

