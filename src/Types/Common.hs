{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types.Common
    ( module Data.Aeson
    , module Data.Aeson.Types
    , module GHC.Generics
    , module Control.Applicative
    , Text
    , unpack
    , pack
    , decodingOptions
    , (<>)
    , Monoid (..)
    , Snowflake (..)
    , Mention
    , Application
    , Emoji
    , VoiceState
    , Timestamp
    , UnixTimestamp
    , Permissions
    , Word64
    , Timestamps (..)
    , Party (..)
    , Assets (..)
    , Role (..)
    , Activity (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid      (Monoid (..), (<>))
import           Data.Text        (Text, pack, unpack)
import           Data.Word        (Word64)
import           GHC.Generics
import Control.Applicative

import           Web.HttpApiData  (ToHttpApiData (..))

newtype Snowflake = Snowflake Word64
    deriving (Show, Eq, Generic)

instance ToJSON Snowflake
instance FromJSON Snowflake where
    parseJSON = withText "Snowflake" $ \s -> do
        return $ Snowflake (read $ unpack s)

instance ToHttpApiData Snowflake where
    toUrlPiece (Snowflake x) = pack $ show x

data Timestamps
    = Timestamps
    { start :: UnixTimestamp
    , end   :: UnixTimestamp
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
    { _id         :: Snowflake
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
    , applicationId :: Maybe Snowflake
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


type Mention = Value
type Application = Value
type Emoji = Value
type VoiceState = Value
type Timestamp = Value
type UnixTimestamp = Word64
type Permissions = Word64



decodingOptions :: Options
decodingOptions =
    defaultOptions
    { sumEncoding        = UntaggedValue
    , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
    , omitNothingFields  = True
    }

