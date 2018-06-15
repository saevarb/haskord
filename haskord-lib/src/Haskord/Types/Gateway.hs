module Haskord.Types.Gateway
    ( GatewayOpcode (..)
    , EventType (..)
    , GatewayCommand (..)
    , Hello' (..)
    , IdentifyPayload (..)
    , IdentifyProperties (..)
    , GatewayResponse (..)
    , Resume' (..)
    , module Haskord.Types.GatewayOpcode
    , module Haskord.Types.GatewayEventType
    ) where

import           Data.Text                      (Text)
import           GHC.Generics

import           Data.Aeson
import           Network.WebSockets             (WebSocketsData (..))

import           Haskord.Types.Common
import           Haskord.Types.GatewayEventType
import           Haskord.Types.GatewayOpcode

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
    | HelloCmd Hello'
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
data Hello'
    = Hello'
    { heartbeatInterval :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON Hello' where
instance FromJSON Hello' where
    parseJSON = genericParseJSON decodingOptions
