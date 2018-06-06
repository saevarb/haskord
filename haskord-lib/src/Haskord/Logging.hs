module Haskord.Logging
    ( BoundedLog
    , Severity (..)
    , LogMessage (..)
    , renderPayload
    , insert
    , Haskord.Logging.empty
    , toList
    ) where

import           Haskord.Prelude

import           Text.Pretty.Simple

data BoundedLog a
    = BoundedLog
    { logMaxItems :: !Int
    , logItems    :: ![a]
    } deriving (Functor, Foldable, Traversable, Show, Eq)

data Severity
    = Info
    | Warning
    | Error
    | Fatal
    deriving (Eq, Show, Enum, Ord)

data LogMessage
    = forall a. (Show a) => LogMessage
    { severity :: Severity
    , title    :: Text
    , payload  :: Maybe a
    }

deriving instance Show LogMessage

renderPayload :: LogMessage -> Maybe Text
renderPayload (LogMessage _ _ p) =
    toStrict . pShowNoColor <$> p


insert :: a -> BoundedLog a -> BoundedLog a
insert a lg@BoundedLog {..} =
    lg { logItems = take logMaxItems $ a : logItems }

empty :: Int -> BoundedLog a
empty n = BoundedLog n []

toList :: BoundedLog a -> [a]
toList = logItems




