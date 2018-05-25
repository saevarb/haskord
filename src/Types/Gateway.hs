{-# LANGUAGE RecordWildCards #-}
module Types.Gateway where

import Control.Monad
import           Data.Text            (Text)
import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific (floatingOrInteger)
import           Network.WebSockets (Connection, DataMessage (..),
                                     WebSocketsData (..))

import Types.Common
import Types.Channel
import Types.Guild
import Types.User

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
        case lookup opcode reverseOpcodeMap of
            Just op -> toJSON op
            Nothing -> error $ "Couldn't find opcode " ++ show opcode
        -- toJSON $ fromJust (lookup opcode reverseOpcodeMap)

data EventType
    = READY
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

data RawGatewayCommand
    = RawGatewayCommand
    { op :: GatewayOpcode
    , d  :: Maybe Value
    , s  :: Maybe Int
    , t  :: Maybe EventType
    } deriving (Generic, Show)

instance ToJSON RawGatewayCommand
instance FromJSON RawGatewayCommand

data GatewayCommand
    = DispatchCmd EventType (Maybe Int) DispatchPayload
    | HeartbeatCmd
    | IdentifyCmd IdentifyPayload
    | StatusUpdateCmd
    | VoiceServerPingCmd
    | ResumeCmd Value
    | ReconnectCmd
    | RequestGuildMembersCmd
    | InvalidSessionCmd
    | HelloCmd Heartbeat'
    | HeartbeatACKCmd
    deriving (Show, Eq)

instance ToJSON GatewayCommand where
    toJSON (DispatchCmd et seqno p) =
        toJSON $ RawGatewayCommand Dispatch (Just $ toJSON p) seqno (Just et)
    toJSON HeartbeatCmd =
        toJSON $ RawGatewayCommand Heartbeat Nothing Nothing Nothing
    toJSON (IdentifyCmd p) =
        toJSON $ RawGatewayCommand Identify (Just $ toJSON p) Nothing Nothing

instance WebSocketsData GatewayCommand where
    toLazyByteString = encode

rawToCommand :: RawGatewayCommand -> Either String GatewayCommand
rawToCommand (RawGatewayCommand {..}) =
    case op of
        Dispatch -> do
            payload <- mte "payload" d
            event <- mte "event type" t
            DispatchCmd event s <$> parseEither (payloadMap event) payload
        Hello -> do
            payload <- mte "payload" d
            HelloCmd <$> parseEither parseJSON payload
        HeartbeatACK ->
            return HeartbeatACKCmd
        _ ->
            mzero
  where
    mte _ (Just v) = Right v
    mte e _        = Left $ "Missing: " ++ e


payloadMap :: EventType -> Value -> Parser DispatchPayload
-- payloadMap HELLO                       val = HelloEvent <$> parseJSON val
-- payloadMap RESUMED                     val = ResumedEvent <$> parseJSON val
-- payloadMap INVALID_SESSION             val = InvalidSessionEvent <$> parseJSON val
payloadMap READY                       val = ReadyEvent <$> parseJSON val
payloadMap CHANNEL_CREATE              val = ChannelCreateEvent <$> parseJSON val
payloadMap CHANNEL_UPDATE              val = ChannelUpdateEvent <$> parseJSON val
payloadMap CHANNEL_DELETE              val = ChannelDeleteEvent <$> parseJSON val
payloadMap CHANNEL_PINS_UPDATE         val = ChannelPinsUpdateEvent <$> parseJSON val
payloadMap GUILD_CREATE                val = GuildCreateEvent <$> parseJSON val
payloadMap GUILD_UPDATE                val = GuildUpdateEvent <$> parseJSON val
payloadMap GUILD_DELETE                val = GuildDeleteEvent <$> parseJSON val
payloadMap GUILD_BAN_ADD               val = GuildBanAddEvent <$> parseJSON val
payloadMap GUILD_BAN_REMOVE            val = GuildBanRemoveEvent <$> parseJSON val
payloadMap GUILD_EMOJIS_UPDATE         val = GuildEmojisUpdateEvent <$> parseJSON val
payloadMap GUILD_INTEGRATIONS_UPDATE   val = GuildIntegrationsUpdateEvent <$> parseJSON val
payloadMap GUILD_MEMBER_ADD            val = GuildMemberAddEvent <$> parseJSON val
payloadMap GUILD_MEMBER_REMOVE         val = GuildMemberRemoveEvent <$> parseJSON val
payloadMap GUILD_MEMBER_UPDATE         val = GuildMemberUpdateEvent <$> parseJSON val
payloadMap GUILD_MEMBERS_CHUNK         val = GuildMembersChunkEvent <$> parseJSON val
payloadMap GUILD_ROLE_CREATE           val = GuildRoleCreateEvent <$> parseJSON val
payloadMap GUILD_ROLE_UPDATE           val = GuildRoleUpdateEvent <$> parseJSON val
payloadMap GUILD_ROLE_DELETE           val = GuildRoleDeleteEvent <$> parseJSON val
payloadMap MESSAGE_CREATE              val = MessageCreateEvent <$> parseJSON val
payloadMap MESSAGE_UPDATE              val = MessageUpdateEvent <$> parseJSON val
payloadMap MESSAGE_DELETE              val = MessageDeleteEvent <$> parseJSON val
payloadMap MESSAGE_DELETE_BULK         val = MessageDeleteBulkEvent <$> parseJSON val
payloadMap MESSAGE_REACTION_ADD        val = MessageReactionAddEvent <$> parseJSON val
payloadMap MESSAGE_REACTION_REMOVE     val = MessageReactionRemoveEvent <$> parseJSON val
payloadMap MESSAGE_REACTION_REMOVE_ALL val = MessageReactionRemoveAllEvent <$> parseJSON val
payloadMap PRESENCE_UPDATE             val = PresenceUpdateEvent <$> parseJSON val
payloadMap TYPING_START                val = TypingStartEvent <$> parseJSON val
payloadMap USER_UPDATE                 val = UserUpdateEvent <$> parseJSON val
payloadMap VOICE_STATE_UPDATE          val = VoiceStateUpdateEvent <$> parseJSON val
payloadMap VOICE_SERVER_UPDATE         val = VoiceServerUpdateEvent <$> parseJSON val
payloadMap WEBHOOKS_UPDATE             val = WebhooksUpdateEvent <$> parseJSON val

-- myParse
-- myParseJson = genericParseJSON decodingOptions

type Resumed = Value
type InvalidSession = Value
type PinsUpdate = Value
type UserBan = Value
type GuildEmojiUpdate = Value
type GuildMemberAdd = Value
type GuildIntegrationUpdate = Value
type GuildMemberUpdate = Value
type GuildRole = Value
type GuildMembersRequest = Value
type WebhooksUpdate = Value
type MessageBulkDelete = Value
type MessageReactionRemoveAll = Value
type VoiceServerUpdate = Value
type Ready = Value

data DispatchPayload
    = ReadyEvent Ready
    | ChannelCreateEvent Channel
    | ChannelUpdateEvent Channel
    | ChannelDeleteEvent Channel
    | ChannelPinsUpdateEvent PinsUpdate
    | GuildCreateEvent Guild
    | GuildUpdateEvent Guild
    | GuildDeleteEvent UnavailableGuild
    | GuildBanAddEvent UserBan
    | GuildBanRemoveEvent UserBan
    | GuildEmojisUpdateEvent GuildEmojiUpdate
    | GuildIntegrationsUpdateEvent GuildIntegrationUpdate
    | GuildMemberAddEvent GuildMemberAdd
    | GuildMemberRemoveEvent UserBan
    | GuildMemberUpdateEvent GuildMemberUpdate
    | GuildMembersChunkEvent GuildMembersRequest
    | GuildRoleCreateEvent GuildRole
    | GuildRoleUpdateEvent GuildRole
    | GuildRoleDeleteEvent GuildRole
    | MessageCreateEvent Message
    | MessageUpdateEvent (Partial Message)
    | MessageDeleteEvent (Partial Message)
    | MessageDeleteBulkEvent MessageBulkDelete
    | MessageReactionAddEvent Reaction
    | MessageReactionRemoveEvent Reaction
    | MessageReactionRemoveAllEvent MessageReactionRemoveAll
    | PresenceUpdateEvent PresenceUpdate
    | TypingStartEvent TypingStart
    | UserUpdateEvent User
    | VoiceStateUpdateEvent VoiceState
    | VoiceServerUpdateEvent VoiceServerUpdate
    | WebhooksUpdateEvent WebhooksUpdate
    -- | UnknownSoFar Value
    deriving (Generic, Show, Eq)


instance ToJSON DispatchPayload where
    toJSON = genericToJSON decodingOptions
instance FromJSON DispatchPayload where
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

data IdentifyPayload
    = IdentifyPayload
    { token          :: Text
    , properties     :: IdentifyProperties
    , compress       :: Maybe Bool
    , largeThreshold :: Maybe Int
    , shard          :: Maybe (Int, Int)
    , presence       :: Maybe Presence
    } deriving (Eq, Show, Generic)

instance ToJSON IdentifyPayload where
    toJSON = genericToJSON decodingOptions
instance FromJSON IdentifyPayload where
    parseJSON = genericParseJSON decodingOptions

data Heartbeat'
    = Heartbeat'
    { heartbeatInterval :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON Heartbeat' where
    toJSON = genericToJSON decodingOptions
instance FromJSON Heartbeat' where
    parseJSON = genericParseJSON decodingOptions
