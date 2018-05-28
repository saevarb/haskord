{-# LANGUAGE PolyKinds #-}
module Plugins
    ( Plugin (..)
    , RunnablePlugin (..)
    , Convertible ()
    , PayloadType (..)
    , BotM (..)
    , Text (..)
    , magic
    , simplePlugin
    , runPlugins
    , module Types.Gateway
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import GHC.TypeLits as GTL

import Types
import Types.Common
import Types.Gateway

class FromJSON (PayloadType ev) => Convertible ev where
    data PayloadType ev :: *
    convert :: Value -> Maybe (PayloadType ev)
    convert = parseMaybe parseJSON


data Plugin (name :: Symbol) (ev :: k) s
    = Convertible ev => Plugin
    { initializePlugin :: BotM s
    , runPlugin :: PayloadType ev -> BotM ()
    }

instance Convertible 'MESSAGE_CREATE where
    data PayloadType 'MESSAGE_CREATE
        = MessageCreatePayload Message
        deriving (Generic, Show, Eq)

instance ToJSON (PayloadType 'MESSAGE_CREATE)
instance FromJSON (PayloadType 'MESSAGE_CREATE)


instance Convertible 'PRESENCE_UPDATE where
    data PayloadType 'PRESENCE_UPDATE
        = PresenceUpdatePayload PresenceUpdate
        deriving (Generic, Show, Eq)

instance ToJSON (PayloadType 'PRESENCE_UPDATE)
instance FromJSON (PayloadType 'PRESENCE_UPDATE)

instance Convertible 'READY where
    data PayloadType 'READY
        = ReadyPayload Ready
        deriving (Generic, Show, Eq)

instance ToJSON (PayloadType 'READY)
instance FromJSON (PayloadType 'READY)

instance Convertible 'Hello where
    data PayloadType 'Hello
        = HelloPayload Heartbeat'
        deriving (Generic, Show, Eq)

instance ToJSON (PayloadType 'Hello)
instance FromJSON (PayloadType 'Hello)


data RunnablePlugin = RunnablePlugin { name :: String, plugin :: (Hide Plugin)}
data Hide f = forall (name :: Symbol) (ev :: k) s. Convertible ev => Hide (f name ev s)

magic :: forall name s ev. (KnownSymbol name, Convertible ev) => Plugin name ev s -> RunnablePlugin
magic p = RunnablePlugin { name = symbolVal (Proxy @name), plugin = Hide p}

run :: Convertible ev => Plugin name ev s -> Value -> BotM ()
run p v =
    case convert v of
        Just v' -> runPlugin p v'
        Nothing -> return ()

simplePlugin :: Convertible ev => (PayloadType ev -> BotM ()) -> Plugin name ev ()
simplePlugin f =
    Plugin
    { runPlugin = f
    , initializePlugin = return ()
    }

runPlugins :: [RunnablePlugin] -> Value -> BotM ()
runPlugins plugs val = do
    forM_ plugs $ \(RunnablePlugin _ (Hide p)) -> do
        run p val
        return ()
