{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
module PluginTest where


import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import GHC.TypeLits as GTL
import Data.Foldable
import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  data Opcode = One | Two | Three
      deriving (Eq, Show, Generic)
   |])

$(singletons [d|
  data Event = Foo | Bar | Baz
      deriving (Eq, Show, Generic)
   |])

instance ToJSON Opcode
instance FromJSON Opcode
instance ToJSON Event
instance FromJSON Event


data Payload :: Opcode -> Maybe Event -> * where
    OnePayload :: OneMsg -> Payload 'One 'Nothing
    FooPayload :: FooMsg -> Payload 'Two ('Just 'Foo)

deriving instance Show (Payload opcode event)


data OneMsg = OneMsg { one :: String }
    deriving (Eq, Show, Generic)
instance ToJSON OneMsg
instance FromJSON OneMsg

data FooMsg = FooMsg { foo :: String }
    deriving (Eq, Show, Generic)
instance ToJSON   FooMsg
instance FromJSON FooMsg

data RawMessage
    = RawMessage
    { opcode  :: Opcode
    , event   :: Maybe Event
    , payload :: Maybe Value
    } deriving (Eq, Show, Generic)

instance ToJSON RawMessage
instance FromJSON RawMessage

data Plugin opcode event s = Plugin
  { initializePlugin :: IO s
  , runPlugin :: Payload opcode event -> IO ()
  }

data RunnablePlugin =
    forall opcode event s.
    RunnablePlugin (Sing opcode) (Sing event) (Plugin opcode event s)

runnablePlugin :: forall opcode event s. (SingI opcode, SingI event) => Plugin opcode event s -> RunnablePlugin
runnablePlugin = RunnablePlugin sing sing

plugins :: [RunnablePlugin]
plugins =
    [runnablePlugin  fooPlugin]


fooPlugin :: Plugin 'Two ('Just 'Foo) ()
fooPlugin =
    Plugin
    { initializePlugin = return ()
    , runPlugin = \(FooPayload _) -> print "foo plugin"
    }

parseEventPayload :: forall opcode event. Sing opcode -> Sing event -> Value -> Parser (Payload opcode event)
parseEventPayload SOne SNothing val = OnePayload <$> parseJSON val
parseEventPayload STwo (SJust SFoo) val = FooPayload <$> parseJSON val

instance (SingI a, SingI b) => FromJSON (Payload a b) where
    parseJSON =
        parseEventPayload sing sing


instance FromJSON SomeMessage where
    parseJSON = withObject "SomeMessage" $ \v -> do
        opcode <- v .: "opcode"
        event <- v .: "event"
        payload <- v .: "payload"
        withSomeSing opcode $ \sopcode ->
            withSomeSing event $ \sevent ->
            SomeMessage <$> parseEventPayload sopcode sevent payload


data SomeMessage = forall opcode event. SomeMessage { p :: Payload opcode event}
test1 :: RawMessage
test1 = RawMessage One Nothing (Just $ toJSON (OneMsg "one"))
test2 :: RawMessage
test2 = RawMessage Two (Just Foo) (Just $ toJSON (FooMsg "one"))

payloadEventType :: Payload op ev -> Sing ev
payloadEventType (OnePayload _) = sing
payloadEventType (FooPayload _) = sing

payloadOpcodeType :: Payload op ev -> Sing op
payloadOpcodeType (OnePayload _) = sing
payloadOpcodeType (FooPayload _) = sing

run :: SomeMessage -> RunnablePlugin -> IO ()
run (SomeMessage py) (RunnablePlugin sop sev pg) =
    case (payloadEventType py %~ sev, payloadOpcodeType py %~ sop) of
        (Proved Refl, Proved Refl) -> runPlugin pg py
        _ -> return ()

runPlugins :: SomeMessage -> [RunnablePlugin] -> IO ()
runPlugins = traverse_ . run
  -- Nothing -> return ()
-- blah :: RawMessage -> Either String (Maybe SomeMessage)
-- blah (RawMessage {..}) = do
--     withSomeSing opcode $ \sopcode ->
--         withSomeSing event $ \sevent ->
--             traverse (parseEither parseJSON) payload



-- instance FromJSON Message where
--     parseJSON = withObject "Message" $ \o -> undefined

