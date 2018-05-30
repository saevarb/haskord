{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
module Plugins
    ( Plugin (..)
    , RunnablePlugin (..)
    , Payload (..)
    , BotM (..)
    , DispatchPlugin (..)
    , RawPlugin (..)
    , DispatchPayload
    , RawPayload
    , SomeMessage (..)
    , runnablePlugin
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
import Data.Singletons.Prelude
import Data.Singletons.TH

import Types
import Types.Common
import Types.Gateway


type DispatchPayload b = Payload 'Dispatch ('Just b)
type RawPayload b = Payload b 'Nothing
type DispatchPlugin b = Plugin 'Dispatch ('Just b)
type RawPlugin b = Plugin b 'Nothing

data Payload :: GatewayOpcode -> Maybe EventType -> * where
    -- MessageCreatePayload :: Message -> Payload 'Dispatch ('Just 'MESSAGE_CREATE)
    HelloPayload         :: Heartbeat' -> RawPayload 'Hello
    MessageCreatePayload :: Message -> DispatchPayload 'MESSAGE_CREATE
    ReadyPayload         :: Ready   -> DispatchPayload 'READY

deriving instance Show (Payload opcode event)


data Plugin opcode event s = Plugin
  { initializePlugin :: BotM s
  , runPlugin :: Payload opcode event -> BotM ()
  }

data RunnablePlugin =
    forall opcode event s.
    RunnablePlugin (Sing opcode) (Sing event) (Plugin opcode event s)

runnablePlugin :: forall opcode event s. (SingI opcode, SingI event) => Plugin opcode event s -> RunnablePlugin
runnablePlugin = RunnablePlugin sing sing

parseEventPayload :: forall opcode event. Sing opcode -> Sing event -> Value -> Parser (Payload opcode event)
parseEventPayload SDispatch (SJust SMESSAGE_CREATE) val = MessageCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SREADY) val = ReadyPayload <$> parseJSON val
parseEventPayload SHello SNothing val = HelloPayload <$> parseJSON val
parseEventPayload _ _ _ = fail "Can't parse payload"

instance (SingI a, SingI b) => FromJSON (Payload a b) where
    parseJSON =
        parseEventPayload sing sing


instance FromJSON SomeMessage where
    parseJSON = withObject "SomeMessage" $ \v -> do
        opcode <- v .: "op"
        event <- v .: "t"
        payload <- v .: "d"
        s <- v .: "s"
        withSomeSing opcode $ \sopcode ->
            withSomeSing event $ \sevent ->
            SomeMessage s <$> parseEventPayload sopcode sevent payload


data SomeMessage
    = forall opcode event.
    SomeMessage { seqNo :: Maybe Int, p :: Payload opcode event}


simplePlugin :: (Payload opcode event -> BotM ()) -> Plugin opcode event ()
simplePlugin f =
    Plugin
    { initializePlugin = return ()
    , runPlugin = f
    }

payloadEventType :: Payload op ev -> Sing ev
payloadEventType (MessageCreatePayload _) = sing
payloadEventType (ReadyPayload _) = sing
payloadEventType (HelloPayload _) = sing

payloadOpcodeType :: Payload op ev -> Sing op
payloadOpcodeType (MessageCreatePayload _) = sing
payloadOpcodeType (ReadyPayload _) = sing
payloadOpcodeType (HelloPayload _) = sing

run :: SomeMessage -> RunnablePlugin -> BotM ()
run (SomeMessage _ py) (RunnablePlugin sop sev pg) =
    case (payloadEventType py %~ sev, payloadOpcodeType py %~ sop) of
        (Proved Refl, Proved Refl) -> runPlugin pg py
        _ -> return ()

runPlugins :: [RunnablePlugin] -> SomeMessage -> BotM ()
runPlugins plugs msg = mapM_ (run msg) plugs
