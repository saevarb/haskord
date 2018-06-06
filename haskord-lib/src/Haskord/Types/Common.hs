{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}
module Haskord.Types.Common
    ( module GHC.Generics
    , Text
    , unpack
    , pack
    , decodingOptions
    , (<>)
    , Monoid (..)
    , Snowflake (..)
    , MessageApplication (..)
    , Emoji (..)
    , Word64
    , Timestamps (..)
    , Party (..)
    , Assets (..)
    , Role (..)
    , Activity (..)
    , User (..)
    , Webhook
    , Guild (..)
    , GuildMember (..)
    , Channel (..)
    , UnavailableGuild (..)
    , Ready (..)
    , Message (..)
    , MessageType (..)
    , Reaction (..)
    , PresenceUpdate (..)
    , TypingStart (..)
    , Presence (..)
    , OutMessage (..)
    , Partial (..)
    , MessageActivity (..)
    , Resumed
    , InvalidSession
    , PinsUpdate
    , UserBan
    , GuildEmojiUpdate
    , GuildMemberAdd
    , GuildIntegrationUpdate
    , GuildMemberUpdate
    , GuildRole
    , GuildMembersRequest
    , WebhooksUpdate
    , MessageBulkDelete
    , MessageReactionRemoveAll
    , VoiceServerUpdate
    , VoiceState
    , Timestamp
    , UnixTimestamp
    , Permissions
    , embedTitle
    , embedDesc
    , embedField
    , embedIField
    , msgText
    , msgEmbed
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Hashable
import           Data.Monoid         (Monoid (..), (<>))
import           Data.Scientific     (toBoundedInteger)
import           Data.Text           (Text, pack, unpack)
import           Data.Word           (Word64)
import           GHC.Generics

import           Web.HttpApiData     (ToHttpApiData (..))

newtype Snowflake (p :: *) = Snowflake Word64
    deriving (Show, Eq, Generic)

instance ToJSON (Snowflake p) where
    toJSON (Snowflake x) = String (pack $ show x)
instance FromJSON (Snowflake p) where
    parseJSON = withText "Snowflake" $ \s ->
        return $ Snowflake (read $ unpack s)

instance ToHttpApiData (Snowflake p) where
    toUrlPiece (Snowflake x) = pack $ show x

instance Hashable (Snowflake p) where
    hashWithSalt s (Snowflake inner) = hashWithSalt s inner

data Timestamps
    = Timestamps
    { start :: Maybe UnixTimestamp
    , end   :: Maybe UnixTimestamp
    } deriving (Eq, Show, Generic)

instance ToJSON Timestamps where
    toJSON = genericToJSON decodingOptions
instance FromJSON Timestamps where
    parseJSON = genericParseJSON decodingOptions

data Party
    = Party
    { id   :: Maybe Text
    , size :: Maybe (Int, Int)
    } deriving (Eq, Show, Generic)

instance ToJSON Party where
    toJSON = genericToJSON decodingOptions
instance FromJSON Party where
    parseJSON = genericParseJSON decodingOptions

data Assets
    = Assets
    { largeImage :: Maybe Text
    , largeText  :: Maybe Text
    , smallImage :: Maybe Text
    , smallText  :: Maybe Text
    } deriving (Generic, Eq, Show)

instance ToJSON Assets where
    toJSON = genericToJSON decodingOptions
instance FromJSON Assets where
    parseJSON = genericParseJSON decodingOptions

data Role
    = Role
    { _id         :: Snowflake Role
    , name        :: Text
    , _color      :: Word64
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

data Activity
    = Activity
    { name          :: Text
    , type_         :: Word64
    , url           :: Maybe Text
    , timestamps    :: Maybe Timestamps
    , applicationId :: Maybe (Snowflake MessageApplication)
    , details       :: Maybe Text
    , state         :: Maybe Text
    , party         :: Maybe Party
    , assets        :: Maybe Assets
    , _instance     :: Maybe Bool
    , flags         :: Maybe Int
    } deriving (Generic, Eq, Show)

instance ToJSON Activity where
    toJSON = genericToJSON decodingOptions
instance FromJSON Activity where
    parseJSON = genericParseJSON decodingOptions

data User
    = User
    { id_           :: Snowflake User
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

data ChannelType
    = GuildText
    | DM
    | GuildVoice
    | GroupDM
    | GuildCategory
    deriving (Eq, Generic, Show, Enum)

instance ToJSON ChannelType where
    toJSON t = Number $ fromIntegral (fromEnum t)
instance FromJSON ChannelType where
    parseJSON = withScientific "ChannelType"  $ \n ->
        case toBoundedInteger n of
            Just v  -> return $ toEnum v
            Nothing -> fail $ "Invalid ChannelType: " ++ show n

data Channel
    = Channel
    { id_                  :: Snowflake Channel
    , type_                :: ChannelType
    , guildId              :: Maybe (Snowflake Guild)
    , position             :: Maybe Int
    , permissionOverwrites :: Maybe [Overwrite]
    , name                 :: Maybe Text
    , topic                :: Maybe Text
    , nsfw                 :: Maybe Bool
    , lastMessageId        :: Maybe (Snowflake Message)
    , bitrate              :: Maybe Int
    , userLimit            :: Maybe Int
    , recipients           :: Maybe [User]
    , icon                 :: Maybe Text
    , ownerId              :: Maybe (Snowflake User)
    , applicationId        :: Maybe (Snowflake MessageApplication)
    , parentId             :: Maybe (Snowflake Channel) -- TODO: Probably not right
    , lastPinTimestamp     :: Maybe Timestamp
    } deriving (Eq, Generic, Show)

instance ToJSON Channel where
    toJSON = genericToJSON decodingOptions
instance FromJSON Channel where
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
    , applicationId               :: Maybe (Snowflake MessageApplication)
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
    , presences                   :: Maybe [Partial PresenceUpdate]
    } deriving (Eq, Generic, Show)

instance ToJSON Guild where
    toJSON = genericToJSON decodingOptions
instance FromJSON Guild where
    parseJSON = genericParseJSON decodingOptions

data MessageType
    = Default
    | RecipientAdd
    | RecipientRemove
    | Call
    | ChannelNameChange
    | ChannelIconChange
    | ChannelPinnedMessage
    | GuildMemberJoin
    deriving (Eq, Show, Enum)

instance ToJSON MessageType where
    toJSON t = Number $ fromIntegral (fromEnum t)
instance FromJSON MessageType where
    parseJSON = withScientific "MessageType"  $ \n ->
        case toBoundedInteger n of
            Just v  -> return $ toEnum v
            Nothing -> fail $ "Invalid MessageType: " ++ show n

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
    , mentionRoles    :: [Partial Role]
    , embeds          :: [Embed]
    , reactions       :: Maybe [Reaction]
    , nonce           :: Maybe (Snowflake Message) -- TODO: not sure Message is right parameter
    , pinned          :: Bool
    , webhookId       :: Maybe (Snowflake Webhook)
    , type_           :: MessageType
    , activity        :: Maybe Activity
    , application     :: Maybe MessageApplication
    } deriving (Show, Eq, Generic)

instance ToJSON Message where
    toJSON = genericToJSON decodingOptions
instance FromJSON Message where
    parseJSON = genericParseJSON decodingOptions

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


data UnavailableGuild
    = UnavailableGuild
    { _id         :: Snowflake Guild
    , unavailable :: Bool
    } deriving (Generic, Eq, Show)

instance ToJSON UnavailableGuild where
    toJSON = genericToJSON decodingOptions
instance FromJSON UnavailableGuild where
    parseJSON = genericParseJSON decodingOptions


data Ready
    = Ready
    { v               :: Int
    , user_           :: User
    , privateChannels :: [Channel]
    , guilds          :: [UnavailableGuild]
    , sessionId       :: Text
    } deriving (Generic, Eq, Show)

instance ToJSON Ready where
    toJSON = genericToJSON decodingOptions
instance FromJSON Ready where
    parseJSON = genericParseJSON decodingOptions

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

data Reaction
    = Reaction
    { userId    :: Snowflake User
    , channelId :: Snowflake Channel
    , messageId :: Snowflake Message
    , emoji     :: Partial Emoji
    } deriving (Generic, Eq, Show)

instance ToJSON Reaction where
    toJSON = genericToJSON decodingOptions
instance FromJSON Reaction where
    parseJSON = genericParseJSON decodingOptions

data PresenceUpdate
    = PresenceUpdate
    { user    :: Partial User
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
    , _tts        :: Bool
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

instance ToJSON OutMessage where
    toJSON = genericToJSON decodingOptions
instance FromJSON OutMessage where
    parseJSON = genericParseJSON decodingOptions

defaultOutMessage :: OutMessage
defaultOutMessage =
    OutMessage Nothing False Nothing Nothing Nothing

data MessageApplication
    = MessageApplication
    { id           :: Snowflake MessageApplication
    , coverImage   :: Text
    , description_ :: Text
    , icon         :: Text
    , name         :: Text
    } deriving (Eq, Show, Generic)

instance ToJSON MessageApplication where
    toJSON = genericToJSON decodingOptions
instance FromJSON MessageApplication where

data ActivityType
    = Join
    | Spectate
    | Listen
    | JoinRequest
    deriving (Show, Eq, Enum)

instance ToJSON ActivityType where
    toJSON t = Number $ fromIntegral (fromEnum t)
instance FromJSON ActivityType where
    parseJSON = withScientific "ActivityType"  $ \n ->
        case toBoundedInteger n of
            Just v  -> return $ toEnum v
            Nothing -> fail $ "Invalid ActivityType: " ++ show n

data MessageActivity
    = MessageActivity
    { type_   :: ActivityType
    , partyId :: Maybe (Snowflake Party)
    } deriving (Generic, Eq, Show)

instance ToJSON MessageActivity where
    toJSON = genericToJSON decodingOptions
instance FromJSON MessageActivity where
    parseJSON = genericParseJSON decodingOptions

data Emoji
    = Emoji
    { id_           :: Snowflake Emoji
    , name          :: Text
    , roles         :: Maybe [Snowflake Role]
    , user          :: Maybe User
    , requireColons :: Maybe Bool
    , managed       :: Maybe Bool
    , animated      :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToJSON Emoji where
    toJSON = genericToJSON decodingOptions
instance FromJSON Emoji where
    parseJSON = genericParseJSON decodingOptions

data family Partial a

data instance Partial Role
    = PartialRole
    { id_ :: Snowflake Role
    } deriving (Eq, Show, Generic)

instance ToJSON (Partial Role) where
    toJSON = genericToJSON decodingOptions
instance FromJSON (Partial Role) where
    parseJSON = genericParseJSON decodingOptions

data instance Partial Message
    = PartialMessage
    { id_             :: Snowflake Role
    , channelId       :: Snowflake Channel
    , editedTimestamp :: Maybe Timestamp
    , content         :: Maybe Text
    , mentions        :: Maybe [User]
    , mentionRoles    :: Maybe [Partial Role]
    , embeds          :: Maybe [Embed]
    , reactions       :: Maybe [Reaction]
    , nonce           :: Maybe (Snowflake Message) -- TODO: not sure Message is right parameter
    , pinned          :: Maybe Bool
    , webhookId       :: Maybe (Snowflake Webhook)
    , type_           :: Maybe Int
    , activity        :: Maybe Activity
    , application     :: Maybe MessageApplication
    } deriving (Eq, Show, Generic)

instance ToJSON (Partial Message) where
    toJSON = genericToJSON decodingOptions
instance FromJSON (Partial Message) where
    parseJSON = genericParseJSON decodingOptions

data instance Partial PresenceUpdate
    = PartialPresenceUpdate
    { status :: Text
    , game   :: Maybe Activity
    , user   :: Partial User
    }
    deriving (Show, Eq, Generic)

instance ToJSON (Partial PresenceUpdate) where
    toJSON = genericToJSON decodingOptions
instance FromJSON (Partial PresenceUpdate) where
    parseJSON = genericParseJSON decodingOptions


data instance Partial User
    = PartialUser
    { id_ :: Snowflake User
    } deriving (Eq, Generic, Show)

instance ToJSON (Partial User) where
    toJSON = genericToJSON decodingOptions
instance FromJSON (Partial User) where
    parseJSON = genericParseJSON decodingOptions

data instance Partial Emoji
    = PartialEmoji
    { id_      :: Maybe (Snowflake Emoji)
    , name     :: Text
    , animated :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON (Partial Emoji) where
    toJSON = genericToJSON decodingOptions
instance FromJSON (Partial Emoji) where
    parseJSON = genericParseJSON decodingOptions

decodingOptions :: Options
decodingOptions =
    defaultOptions
    { sumEncoding        = UntaggedValue
    , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
    , omitNothingFields  = True
    }


type VoiceState = Value
type Timestamp = Value
type UnixTimestamp = Word64
type Permissions = Word64
type Webhook = Value
type Overwrite = Value
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
