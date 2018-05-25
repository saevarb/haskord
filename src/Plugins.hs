{-# LANGUAGE AllowAmbiguousTypes #-}
module Plugins where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


import Types
import Types.Common
import Types.Gateway

class FromJSON (PayloadType ev) => Convertible (ev :: EventType) where
    data PayloadType ev :: *
    convert :: Value -> Maybe (PayloadType ev)
    convert = parseMaybe parseJSON

    -- run :: Proxy (PayloadType ev) -> Plugin ev s -> Value -> IO ()
run :: Convertible ev => Plugin ev s -> Value -> BotM ()
run p v =
    case convert v of
        Just v' -> runPlugin p v' -- doesn't typecheck because of non-injectivity of type families
        Nothing -> return ()

data Plugin (ev :: EventType) s
    = Convertible ev => Plugin
    { initializePlugin :: BotM s
    , runPlugin :: PayloadType ev -> BotM ()
    }

instance Convertible 'MESSAGE_CREATE where
    data PayloadType 'MESSAGE_CREATE
        = MessageCreatePayload Message
        deriving (Generic, Show, Eq)

instance Convertible 'PRESENCE_UPDATE where
    data PayloadType 'PRESENCE_UPDATE
        = PresenceUpdatePayload PresenceUpdate
        deriving (Generic, Show, Eq)


instance ToJSON (PayloadType 'PRESENCE_UPDATE)
instance ToJSON (PayloadType 'MESSAGE_CREATE)
instance FromJSON (PayloadType 'PRESENCE_UPDATE)
instance FromJSON (PayloadType 'MESSAGE_CREATE)


newtype RunnablePlugin = RunnablePlugin (Hide Plugin)
data Hide f = forall (ev :: EventType) s. Convertible ev => Hide (f ev s)


runPlugins :: Value -> [RunnablePlugin] -> BotM ()
runPlugins val plugs = do
    forM_ plugs $ \(RunnablePlugin (Hide p)) -> do
        run p val
        return ()


