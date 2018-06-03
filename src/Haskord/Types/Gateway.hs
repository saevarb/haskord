{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Haskord.Types.Gateway where

import           Data.Text            (Text)
import           GHC.Generics

import           Data.Aeson
import           Data.Scientific (floatingOrInteger)
import           Network.WebSockets (WebSocketsData (..))

import Data.Singletons.TH

import Haskord.Types.Common

$(singletons [d|
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
      deriving (Show, Eq)
  |])

opcodeMap :: [(Int, GatewayOpcode)]
opcodeMap =
    zip [0 .. 11] opcodes
  where
    opcodes =
        [ Dispatch
        , Heartbeat
        , Identify
        , StatusUpdate
        , VoiceStateUpdate
        , VoiceServerPing
        , Resume
        , Reconnect
        , RequestGuildMembers
        , InvalidSession
        , Hello
        , HeartbeatACK
        , UnknownError
        , UnknownOpcode
        ]

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


$(singletons [d|
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
      deriving (Show, Eq, Generic)
  |])

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

data Resume'
    = Resume'
    { token_     :: Text
    , sessionId_ :: Text
    , seq_       :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON   Resume' where
    toJSON = genericToJSON decodingOptions
instance FromJSON Resume' where
    parseJSON = genericParseJSON decodingOptions

data GatewayCommand
    = HeartbeatCmd
    | IdentifyCmd IdentifyPayload
    | StatusUpdateCmd
    | VoiceServerPingCmd
    | ResumeCmd Resume'
    | ReconnectCmd
    | RequestGuildMembersCmd
    | InvalidSessionCmd
    | HelloCmd Heartbeat'
    | HeartbeatACKCmd
    deriving (Show, Eq)

instance ToJSON GatewayCommand where
    toJSON HeartbeatCmd =
        toJSON $ RawGatewayCommand Heartbeat Nothing Nothing Nothing
    toJSON (IdentifyCmd p) =
        toJSON $ RawGatewayCommand Identify (Just $ toJSON p) Nothing Nothing
    toJSON (ResumeCmd res) =
        toJSON $ RawGatewayCommand Resume (Just $ toJSON res) Nothing Nothing

instance WebSocketsData GatewayCommand where
    toLazyByteString = encode
    fromDataMessage    = error "Attempt to read GatewayCommand from Websocket -- this shouldn't happen"
    fromLazyByteString = error "Attempt to read GatewayCommand from Websocket -- this shouldn't happen"

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
