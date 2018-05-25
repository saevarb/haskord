{-# LANGUAGE DuplicateRecordFields #-}
module Types.Channel where

import           Data.Aeson

import           Types.Common
import Types.Guild


data Message
    = Message
    { id_             :: Snowflake Message
    , channelId       :: Snowflake Channel
    , author          :: User
    , content         :: Text
    , timestamp       :: Timestamp
    , editedTimestamp :: Maybe Text
    , tts             :: Bool
    , mentionEveryone :: Bool
    , mentions        :: [User]
    , mentionRoles    :: [PartialRole]
    , embeds          :: [Embed]
    , reactions       :: Maybe [Reaction]
    , nonce           :: Maybe (Snowflake Message) -- TODO: not sure Message is right parameter
    , pinned          :: Bool
    , webhookId       :: Maybe (Snowflake Webhook)
    , type_           :: Int
    , activity        :: Maybe Activity
    , application     :: Maybe Application
    } deriving (Show, Eq, Generic)

instance ToJSON Message where
    toJSON = genericToJSON decodingOptions
instance FromJSON Message where
    parseJSON = genericParseJSON decodingOptions


data Reaction
    = Reaction
    { userId    :: Snowflake User
    , channelId :: Snowflake Channel
    , messageId :: Snowflake Message
    , emoji     :: PartialEmoji
    } deriving (Generic, Eq, Show)

type PartialEmoji = Value

instance ToJSON Reaction where
    toJSON = genericToJSON decodingOptions
instance FromJSON Reaction where
    parseJSON = genericParseJSON decodingOptions

data PresenceUpdate
    = PresenceUpdate
    { user    :: PartialUser
    , roles   :: [Snowflake Role]
    , game    :: Maybe Activity
    , guildId :: Snowflake Guild
    , status  :: Text
    } deriving (Generic, Eq, Show)

instance ToJSON PresenceUpdate where
    toJSON = genericToJSON decodingOptions
instance FromJSON PresenceUpdate where
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
    mappend = joinEmbeds
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

embedTitle :: Text -> Embed
embedTitle t = mempty { title = Just t }

embedDesc :: Text -> Embed
embedDesc d = mempty { description = Just d }

embedField :: Text -> Text -> Embed
embedField k v = mempty { fields = Just [EmbedField k v Nothing]}

embedIField :: Text -> Text -> Embed
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
    { name   :: Text
    , value  :: Text
    , inline :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToJSON EmbedField where
    toJSON = genericToJSON decodingOptions
instance FromJSON EmbedField where
    parseJSON = genericParseJSON decodingOptions

data TypingStart
    = TypingStart
    { channelId :: Snowflake Channel
    , userId    :: Snowflake User
    , timestamp :: Timestamp
    , guildId   :: Snowflake Guild
    } deriving (Generic, Eq, Show)

instance ToJSON TypingStart where
    toJSON = genericToJSON decodingOptions
instance FromJSON TypingStart where
    parseJSON = genericParseJSON decodingOptions

data OutMessage
    = OutMessage
    { _content    :: Maybe Text
    , _tts         :: Bool
    , file        :: Maybe Text
    , embed       :: Maybe Embed
    , payloadJson :: Maybe Text
    } deriving (Show, Eq, Generic)


instance Monoid OutMessage where
    mappend = joinMessages
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
    -- isEmbed = isJust (embed m1) || isJust (embed m2)

defaultOutMessage :: OutMessage
defaultOutMessage =
    OutMessage Nothing False Nothing Nothing Nothing

instance ToJSON OutMessage where
    toJSON = genericToJSON decodingOptions
instance FromJSON OutMessage where
    parseJSON = genericParseJSON decodingOptions

type PartialRole = Value
