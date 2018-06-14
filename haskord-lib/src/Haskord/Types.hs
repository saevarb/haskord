{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}
module Haskord.Types
    ( module Haskord.Types.Common
    , module Haskord.Types.Gateway
    , BotState (..)
    , BotM (..)
    , WrappedPlugin (..)
    , DispatchPlugin
    , RawPlugin
    , DispatchPayload
    , RawPayload
    , Plugin (..)
    , Payload (..)
    , SomeMessage (..)
    , BotConfig (..)
    , BotSettings (..)
    , toGateway
    , runDb
    , logI, logW, logE, logF
    , logI', logW', logE', logF'
    , wrapPlugin
    , simplePlugin
    , initializePlugins
    , runPlugins
    , runBotM
    , readConfig
    , (#)
    , withPlugin
    , withPlugins
    , sendMessage
    ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception.Safe
import           Control.Monad.Reader
import Control.Exception (AsyncException (ThreadKilled))
import           Data.Pool
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Yaml
import           Database.Persist.Sqlite
import           GHC.Generics
import           GHC.TypeLits            as GTL
import qualified Haxl.Core.DataCache     as H
import           System.IO.Unsafe        (unsafePerformIO)


import           Haskord.Http
import           Haskord.Logging         as L
import           Haskord.Prelude
import           Haskord.Rendering
import           Haskord.Types.Common
import           Haskord.Types.Gateway


data BotState
    = BotState
    { sessionIdVar      :: TMVar Text
    , seqNoVar          :: TMVar Int
    , heartbeatThreadId :: TMVar (Async ())
    , writerThreadId    :: TMVar (Async ())
    , me                :: TMVar User
    , logVar            :: TVar (BoundedLog LogMessage)
    , gwQueue           :: TQueue GatewayCommand
    , eventChan         :: BChan RenderEvent
    , dbConnPool        :: Pool SqlBackend
    , gatewayUrl        :: String
    , botSettings       :: BotSettings
    , requestCache      :: TVar (H.DataCache CacheResult)
    }

newtype BotM a
    = BotM
    { unBotM :: ReaderT BotState IO a
    } deriving (MonadThrow, MonadCatch, MonadMask, Applicative, Monad, MonadIO, MonadReader BotState, Functor)


data BotConfig
    = BotConfig
    { botToken :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON BotConfig

data BotSettings
    = BotSettings
    { botPlugins :: [WrappedPlugin]
    , botConfig  :: BotConfig
    }

(#) :: a -> (a -> b) -> b
(#) = flip ($)


withPlugin :: WrappedPlugin -> BotSettings -> BotSettings
withPlugin p set@BotSettings {..} =
    set { botPlugins = p : botPlugins }

withPlugins :: [WrappedPlugin] -> BotSettings -> BotSettings
withPlugins = flip (foldr withPlugin)

readConfig :: FilePath -> IO (Either ParseException BotConfig)
readConfig = decodeFileEither

runBotM :: BotState -> BotM a -> IO a
runBotM bs fn =
    runReaderT (unBotM fn) bs


runDb :: SqlPersistT IO a -> BotM a
runDb query = do
    pool <- asks dbConnPool
    liftIO $ runSqlPool query pool

makeLogger
  :: (MonadReader BotState m, MonadIO m) => Severity -> Text -> m ()
makeLogger s e = do
    lv <- asks logVar
    ec <- asks eventChan
    let lm = LogMessage s e (Nothing :: Maybe ())
    liftIO $ do
        writeBChan ec (MessageAdded lm)
        atomically $ modifyTVar lv (L.insert lm)

makeLogger'
  :: (MonadReader BotState m, MonadIO m) => Show p => Severity -> Text -> p -> m ()
makeLogger' s e p = do
    lv <- asks logVar
    ec <- asks eventChan
    let lm = LogMessage s e (Just p)
    liftIO $ do
        writeBChan ec (MessageAdded lm)
        atomically $ modifyTVar lv (L.insert lm)


logI, logW, logE, logF :: (MonadReader BotState m, MonadIO m) => Text -> m ()
logI = makeLogger Info
logW = makeLogger Warning
logE = makeLogger Error
logF = makeLogger Fatal

logI', logW', logE', logF' :: (MonadReader BotState m, MonadIO m, Show p) => Text -> p -> m ()
logI' = makeLogger' Info
logW' = makeLogger' Warning
logE' = makeLogger' Error
logF' = makeLogger' Fatal



toGateway :: GatewayCommand -> BotM ()
toGateway x = do
    q <- asks gwQueue
    liftIO . atomically $ writeTQueue q x



type DispatchPlugin n a  = Plugin n 'Dispatch ('Just a)
type RawPlugin n a       = Plugin n a 'Nothing
data Plugin (name :: Symbol) opcode event s = Plugin
  { initializePlugin :: BotM s
  , runPlugin        :: TVar s -> Payload opcode event -> BotM ()
  }

data WrappedPlugin =
    forall name opcode event s.
    WrappedPlugin
    { opS               :: Sing opcode
    , evS               :: Sing event
    , pluginInitializer :: BotM s
    , pluginState       :: TVar s
    , plugin            :: Plugin name opcode event s
    , pluginName        :: Text
    }


type DispatchPayload a = Payload 'Dispatch ('Just a)
type RawPayload a      = Payload a 'Nothing
data Payload :: GatewayOpcode -> Maybe EventType -> * where
    HelloPayload                    :: Hello'                 -> RawPayload 'Hello
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
    SomeMessage
    { seqNo   :: Maybe Int
    , eventS  :: Sing event
    , opcodeS :: Sing opcode
    , p       :: Payload opcode event
    }

-- deriving instance Show SomeMessage

instance FromJSON SomeMessage where
    parseJSON = withObject "SomeMessage" $ \v -> do
        opcode <- v .: "op"
        event <- v .: "t"
        payload <- v .: "d"
        s <- v .: "s"
        withSomeSing opcode $ \sopcode ->
            withSomeSing event $ \sevent ->
            SomeMessage s sevent sopcode <$> parseEventPayload sopcode sevent payload

{-# NOINLINE wrapPlugin #-}
wrapPlugin
    :: forall name opcode event s.
       (KnownSymbol name, SingI opcode, SingI event)
    => Plugin name opcode event s
    -> WrappedPlugin
wrapPlugin p =
    WrappedPlugin
    { opS = sing
    , evS = sing
    , pluginInitializer = initializePlugin p
    , plugin = p
    -- TODO: Forgive me father, for I have sinned..
    , pluginName = pack $ GTL.symbolVal $ Proxy @name
    }


simplePlugin :: (Payload opcode event -> BotM ()) -> Plugin name opcode event ()
simplePlugin f =
    Plugin
    { initializePlugin = return ()
    , runPlugin = const f
    }

-- Warning: Boilerplate ahead
-- NOTE: This function makes ghc's pattern match checker go nuts.
-- All pattern matching overlapping/exhaustiveness checking has been disabled in this module.
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

run :: SomeMessage -> WrappedPlugin -> BotM ()
run (SomeMessage _ pev pop py) WrappedPlugin {..} =
    -- let (pev, pop) = payloadType py
    case (pev %~ evS, pop  %~ opS) of
        (Proved Refl, Proved Refl) -> do
            runPlugin plugin pluginState py
        _                          -> return ()

runPlugins :: [WrappedPlugin] -> SomeMessage -> BotM ()
runPlugins plugs msg = mapM_ (sandboxPlugin msg) plugs

sandboxPlugin :: SomeMessage -> WrappedPlugin -> BotM ()
sandboxPlugin msg plugin = do
    s <- ask
    void $ liftIO $ async $ runBotM s $ do
        res <- sandbox 5 $ run msg plugin
        case res of
            Left e -> logW' ("Plugin '" <> pluginName plugin <> "' crashed") e
            Right _ -> return ()


initializePlugins :: [WrappedPlugin] -> BotM [WrappedPlugin]
initializePlugins =
    mapM initialize
  where
    initialize (WrappedPlugin {..}) = do
        stateVar <- liftIO . newTVarIO =<< pluginInitializer
        return $ WrappedPlugin { pluginState = stateVar, ..}

sandbox :: Int -> BotM a -> BotM (Either SomeException a)
sandbox duration fn = do
    s <- ask
    liftIO $ do
        sandboxId <- async $ runBotM s fn
        killerId <- async $ threadDelay (duration * 1000000) >> cancel sandboxId
        res <- waitEitherCatchCancel killerId sandboxId
        case res of
            Left (Left e)     -> return $ Left  e
            Left (Right _)    -> return . Left $ SomeException ThreadKilled
            Right (Left e)    -> return $ Left e
            Right (Right val) -> return $ Right val


data CacheResult a
    = Cached a
    | DontCache
    deriving (Show)

runRequest :: (Typeable a, Show a) => (a -> CacheResult a) -> DiscordReq a -> BotM a
runRequest cacheFn r = do
    cacheVar <- asks requestCache
    cache <- liftIO $ readTVarIO cacheVar
    case H.lookup r cache of
        Just (Cached v) ->
            return v
        _ -> do
            res <- fetch r
            liftIO $ atomically $ modifyTVar cacheVar (H.insert r $ cacheFn res)
            return res
  where
    fetch :: DiscordReq a -> BotM a
    fetch req = do
        tok <- asks (botToken . botConfig . botSettings)
        res <- liftIO $ Control.Exception.Safe.try (runDiscordRequest tok req)
        case res of
            Left (e :: SomeException) -> logE' "Haxl request failed" e >> error "TODO: Fix this"
            Right val -> return val

sendMessage :: Snowflake Channel -> OutMessage -> BotM Message
sendMessage channel msg = do
    runRequest (const DontCache) (CreateMessage channel msg)
