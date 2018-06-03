{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
module Haskord.Plugins
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
    , initializePlugins
    , module Haskord.Types
    , module Haskord.Types.Common
    , module Haskord.Types.Gateway
    ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import GHC.TypeLits as GTL
import Data.Singletons.Prelude
import Data.Singletons.TH

import Haskord.Types
import Haskord.Types.Common
import Haskord.Types.Gateway
import Haskord.Sandbox


type DispatchPlugin a  = Plugin 'Dispatch ('Just a)
type RawPlugin a       = Plugin a 'Nothing
data Plugin opcode event s = Plugin
  { initializePlugin :: BotM ()
  , runPlugin        :: Payload opcode event -> BotM ()
  }

type DispatchPayload a = Payload 'Dispatch ('Just a)
type RawPayload a      = Payload a 'Nothing
data Payload :: GatewayOpcode -> Maybe EventType -> * where
    HelloPayload                    :: Heartbeat'             -> RawPayload 'Hello
    ReadyPayload                    :: Ready                  -> DispatchPayload 'READY
    ChannelCreatePayload            :: Channel                -> DispatchPayload 'CHANNEL_CREATE
    ChannelUpdatePayload            :: Channel                -> DispatchPayload 'CHANNEL_UPDATE
    ChannelDeletePayload            :: Channel                -> DispatchPayload 'CHANNEL_DELETE
    ChannelPinsUpdatePayload        :: PinsUpdate             -> DispatchPayload 'CHANNEL_PINS_UPDATE
    GuildCreatePayload              :: Guild                  -> DispatchPayload 'GUILD_CREATE
    GuildUpdatePayload              :: Guild                  -> DispatchPayload 'GUILD_UPDATE
    GuildDeletePayload              :: UnavailableGuild       -> DispatchPayload 'GUILD_DELETE
    GuildBanAddPayload              :: UserBan                -> DispatchPayload 'GUILD_BAN_ADD
    GuildBanRemovePayload           :: UserBan                -> DispatchPayload 'GUILD_BAN_REMOVE
    GuildEmojisUpdatePayload        :: GuildEmojiUpdate       -> DispatchPayload 'GUILD_EMOJIS_UPDATE
    GuildIntegrationsUpdatePayload  :: GuildIntegrationUpdate -> DispatchPayload 'GUILD_INTEGRATIONS_UPDATE
    GuildMemberAddPayload           :: GuildMemberAdd         -> DispatchPayload 'GUILD_MEMBER_ADD
    GuildMemberRemovePayload        :: Value                  -> DispatchPayload 'GUILD_MEMBER_REMOVE
    GuildMemberUpdatePayload        :: GuildMemberUpdate      -> DispatchPayload 'GUILD_MEMBER_UPDATE
    GuildMembersChunkPayload        :: Value                  -> DispatchPayload 'GUILD_MEMBERS_CHUNK
    GuildRoleCreatePayload          :: GuildRole              -> DispatchPayload 'GUILD_ROLE_CREATE
    GuildRoleUpdatePayload          :: GuildRole              -> DispatchPayload 'GUILD_ROLE_UPDATE
    GuildRoleDeletePayload          :: Value                  -> DispatchPayload 'GUILD_ROLE_DELETE
    MessageCreatePayload            :: Message                -> DispatchPayload 'MESSAGE_CREATE
    MessageUpdatePayload            :: Partial Message        -> DispatchPayload 'MESSAGE_UPDATE
    MessageDeletePayload            :: Partial Message        -> DispatchPayload 'MESSAGE_DELETE
    MessageDeleteBulkPayload        :: MessageBulkDelete      -> DispatchPayload 'MESSAGE_DELETE_BULK
    MessageReactionAddPayload       :: Reaction               -> DispatchPayload 'MESSAGE_REACTION_ADD
    MessageReactionRemovePayload    :: Reaction               -> DispatchPayload 'MESSAGE_REACTION_REMOVE
    MessageReactionRemoveAllPayload :: Partial Message        -> DispatchPayload 'MESSAGE_REACTION_REMOVE_ALL
    PresenceUpdatePayload           :: PresenceUpdate         -> DispatchPayload 'PRESENCE_UPDATE 
    TypingStartPayload              :: TypingStart            -> DispatchPayload 'TYPING_START
    UserUpdatePayload               :: User                   -> DispatchPayload 'USER_UPDATE
    VoiceStateUpdatePayload         :: VoiceState             -> DispatchPayload 'VOICE_STATE_UPDATE
    VoiceServerUpdatePayload        :: VoiceServerUpdate      -> DispatchPayload 'VOICE_SERVER_UPDATE
    WebhooksUpdatePayload           :: WebhooksUpdate         -> DispatchPayload 'WEBHOOKS_UPDATE

deriving instance Show (Payload opcode event)
instance (SingI a, SingI b) => FromJSON (Payload a b) where
    parseJSON =
        parseEventPayload sing sing

data SomeMessage
    = forall opcode event.
    SomeMessage { seqNo :: Maybe Int, eventS :: Sing event, opcodeS :: Sing opcode, p :: Payload opcode event}

instance FromJSON SomeMessage where
    parseJSON = withObject "SomeMessage" $ \v -> do
        opcode <- v .: "op"
        event <- v .: "t"
        payload <- v .: "d"
        s <- v .: "s"
        withSomeSing opcode $ \sopcode ->
            withSomeSing event $ \sevent ->
            SomeMessage s sevent sopcode <$> parseEventPayload sopcode sevent payload

data RunnablePlugin =
    forall opcode event s.
    RunnablePlugin (Sing opcode) (Sing event) (BotM ()) (Plugin opcode event s)

runnablePlugin :: forall opcode event s. (SingI opcode, SingI event) => Plugin opcode event s -> RunnablePlugin
runnablePlugin p = RunnablePlugin sing sing (initializePlugin p) p




simplePlugin :: (Payload opcode event -> BotM ()) -> Plugin opcode event ()
simplePlugin f =
    Plugin
    { initializePlugin = return ()
    , runPlugin = f
    }

-- Warning: Boilerplate ahead
parseEventPayload :: forall opcode event. Sing opcode -> Sing event -> Value -> Parser (Payload opcode event)
parseEventPayload SHello    SNothing                val            =
    HelloPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SREADY)                      val =
    ReadyPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SCHANNEL_CREATE)             val =
    ChannelCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SCHANNEL_UPDATE)             val =
    ChannelUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SCHANNEL_DELETE)             val =
    ChannelDeletePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SCHANNEL_PINS_UPDATE)        val =
    ChannelPinsUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_CREATE)               val =
    GuildCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_UPDATE)               val =
    GuildUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_DELETE)               val =
    GuildDeletePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_BAN_ADD)              val =
    GuildBanAddPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_BAN_REMOVE)           val =
    GuildBanRemovePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_EMOJIS_UPDATE)        val =
    GuildEmojisUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_INTEGRATIONS_UPDATE)  val =
    GuildIntegrationsUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_MEMBER_ADD)           val =
    GuildMemberAddPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_MEMBER_REMOVE)        val =
    GuildMemberRemovePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_MEMBER_UPDATE)        val =
    GuildMemberUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_MEMBERS_CHUNK)        val =
    GuildMembersChunkPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_ROLE_CREATE)          val =
    GuildRoleCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_ROLE_UPDATE)          val =
    GuildRoleUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SGUILD_ROLE_DELETE)          val =
    GuildRoleDeletePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_CREATE)             val =
    MessageCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_UPDATE)             val =
    MessageUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_DELETE)             val =
    MessageDeletePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_DELETE_BULK)        val =
    MessageDeleteBulkPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_REACTION_ADD)       val =
    MessageReactionAddPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_REACTION_REMOVE)    val =
    MessageReactionRemovePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SMESSAGE_REACTION_REMOVE_ALL)val =
    MessageReactionRemoveAllPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SPRESENCE_UPDATE )           val =
    PresenceUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust STYPING_START)               val =
    TypingStartPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SUSER_UPDATE)                val =
    UserUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SVOICE_STATE_UPDATE)         val =
    VoiceStateUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SVOICE_SERVER_UPDATE)        val =
    VoiceServerUpdatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SWEBHOOKS_UPDATE)            val =
    WebhooksUpdatePayload <$> parseJSON val
parseEventPayload sop sev val = do
    let errmsg =
            unlines
            [ "Opcode: " ++ show (fromSing sop)
            , "Event: " ++ show (fromSing sev)
            , "Payload:"
            , show val
            ]
    fail errmsg

run :: SomeMessage -> RunnablePlugin -> BotM ()
run (SomeMessage _ pev pop py) (RunnablePlugin sop sev _ pg) =
    -- let (pev, pop) = payloadType py
    case (pev %~ sev, pop  %~ sop) of
        (Proved Refl, Proved Refl) -> runPlugin pg py
        _ -> return ()

runPlugins :: [RunnablePlugin] -> SomeMessage -> BotM ()
runPlugins plugs msg = mapM_ (sandbox 5 . run msg) plugs

initializePlugins :: [RunnablePlugin] -> BotM ()
initializePlugins =
    mapM_ initialize
  where
    initialize (RunnablePlugin _ _ i _) =
        i

