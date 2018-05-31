module Haskord.Logging where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Pretty.Simple

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
    = forall a. Show a => LogMessage
    { severity :: Severity
    , title    :: Text
    , payload  :: Maybe a
    }

renderPayload :: LogMessage -> Text
renderPayload (LogMessage _ _ p) =
    TL.toStrict $ pShowNoColor p


insert :: a -> BoundedLog a -> BoundedLog a
insert a lg@BoundedLog {..} =
    lg { logItems = take logMaxItems $ a : logItems }

empty :: Int -> BoundedLog a
empty n = BoundedLog n []

toList :: BoundedLog a -> [a]
toList = logItems




