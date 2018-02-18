{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Text (Text, unpack, pack)

import Data.Scientific
import Data.Aeson

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
    | DecodeError
    | NotAuthenticated
    | AuthenticationFailed
    | AlreadyAuthenticated
    | InvalidSeq
    | RateLimited
    | SessionTimeout
    | InvalidShard
    | ShardingRequired
    deriving (Show, Eq, Enum)

opcodeMap :: [(Int, GatewayOpcode)]
opcodeMap =
    zip ([0 .. 11] ++ [4000 .. 4011]) [Heartbeat ..]

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
                    maybe (fail "Unknown opcode") (return) $ lookup i opcodeMap

instance ToJSON GatewayOpcode where
    toJSON opcode =
        toJSON $ fromJust (lookup opcode reverseOpcodeMap)

data GatewayMessage payloadType
    = GatewayMessage
    { op :: GatewayOpcode
    , d  :: Maybe payloadType
    , s  :: Maybe Int
    , t  :: Maybe Text
    } deriving (Generic, Show)

instance (ToJSON a) => ToJSON (GatewayMessage a)
instance (FromJSON a) => FromJSON (GatewayMessage a)


data Payload
    = HeartbeatPayload
    { heartbeatInterval :: Int
    }
    -- | UnknownPayload
    deriving (Generic, Show)

instance ToJSON Payload
instance FromJSON Payload where
    parseJSON = genericParseJSON decodingOptions

decodingOptions :: Options
decodingOptions =
    defaultOptions
    { sumEncoding = UntaggedValue
    , fieldLabelModifier = camelTo2 '_'
    }


data GatewayResponse
    = GatewayResponse
    { url :: Text
    , shards :: Maybe Int
    } deriving (Show, Generic)


instance ToJSON GatewayResponse
instance FromJSON GatewayResponse


-- mkHeartbeat :: (Maybe Int) -> _
mkHeartbeat :: ToJSON payloadType => Maybe payloadType -> ByteString
mkHeartbeat v =
    -- encode $ object [ "op" .= (1 :: Int), "d" .= v ]
    encode $ GatewayMessage { d = v, op = Heartbeat, t = Nothing, s = Nothing}
