{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Text (Text, unpack, pack)
import Data.Semigroup

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
    , fieldLabelModifier = camelTo2 '_'
    , omitNothingFields = True
    }

data GatewayMessage payloadType
    = GatewayMessage
    { op :: GatewayOpcode
    , d  :: Maybe payloadType
    , s  :: Maybe Int
    , t  :: Maybe Text
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


data Activity
    = Activity
    deriving (Show, Eq, Generic)

instance ToJSON Activity
instance FromJSON Activity

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
    deriving (Generic, Show)

instance ToJSON Payload where
    toJSON = genericToJSON decodingOptions
instance FromJSON Payload where
    parseJSON = genericParseJSON decodingOptions

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
