{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskord.Plugins.Default
    ( defaultPlugins
    ) where

import           Control.Monad
import           Control.Monad.Reader.Class

import           Streaming                  as S
import qualified Streaming.Prelude          as S
import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Types
import qualified Data.Text as T
import Data.List (find)

import           Haskord.Prelude
import           Haskord.Types

defaultPlugins :: [WrappedPlugin]
defaultPlugins =
    [ wrapPlugin helloPlugin
    , wrapPlugin readyPlugin
    , wrapPlugin chatLoggerPlugin
    , wrapPlugin helpPlugin
    ]

chatLoggerPlugin :: DispatchPlugin "Chat logger" 'MESSAGE_CREATE ()
chatLoggerPlugin =
    simplePlugin $ \(MessageCreatePayload Message {..}) ->
        logI' ("Message from " <> username author) content

readyPlugin :: DispatchPlugin "Ready handler" 'READY ()
readyPlugin =
    simplePlugin $ \(ReadyPayload ready) -> do
       sessVar <- asks sessionIdVar
       meVar <- asks me
       liftIO . atomically $ putTMVar meVar (user_  ready)
       liftIO $ atomically $ do
           isEmpty <- isEmptyTMVar sessVar
           if isEmpty then
               void $ putTMVar sessVar (sessionId ready)
               else
               void $ swapTMVar sessVar (sessionId ready)
       logI' "Ready received" ready

helloPlugin :: RawPlugin "Hello handler" 'Hello ()
helloPlugin =
    simplePlugin readyHandler
  where
    readyHandler :: RawPayload 'Hello -> BotM ()
    readyHandler (HelloPayload hello) = do
        token <- asks (botToken . botConfig . botSettings)
        meVar <- asks me
        notIdentified <- liftIO . atomically $ isEmptyTMVar meVar
        when notIdentified $
            toGateway $ IdentifyCmd $ identPayload token
        startHeartbeatThread $ heartbeatInterval hello

    startHeartbeatThread :: Int -> BotM ()
    startHeartbeatThread interval = do
        logI "Starting heartbeat thread.."
        gwq <- asks gwQueue
        htidvar <- asks heartbeatThreadId
        htid <- liftIO $ atomically $ tryTakeTMVar htidvar
        case htid of
            Just n  -> liftIO $ cancel n
            Nothing -> return ()
        tid <- liftIO $ async $
            queueSink gwq
            $ S.delay (fromIntegral interval / 1000)
            $ S.repeat HeartbeatCmd
        liftIO . atomically $ putTMVar htidvar tid

    identPayload :: Text -> IdentifyPayload
    identPayload token =
        IdentifyPayload
        { token          = token
        , properties     = IdentifyProperties "linux" "disco" "disco"
        , compress       = Nothing
        , largeThreshold = Nothing
        , shard          = Nothing
        , presence       = Nothing
        }


queueSink :: TQueue a -> Stream (Of a) IO r -> IO r
queueSink q =
    S.mapM_ (liftIO . atomically . writeTQueue q)


data Help
    = Help Text [Text]
    | HelpAll
    deriving (Show)

helpParser :: ParserInfo Help
helpParser =
    info ((helpP <|> helpAllP) <**> helper) (progDesc "Help plugin for other plugins")
  where
    helpP =
        Help
        <$> strArgument (metavar "PLUGIN_NAME" <> help "The name of the plugin")
        <*> many (strArgument (metavar "SUBCOMMAND" <> help "Possible subcommands for the plugin"))
    helpAllP =
        pure HelpAll

helpPlugin :: CommandPlugin "help" ()
helpPlugin =
    commandPlugin
    (return ())
    (CommandHandler handler helpParser)
  where
    handler _ Message {..} HelpAll = do
        plugs <- asks (botPlugins . botSettings)
        let desc = flip foldMap plugs $ \WrappedPlugin {..} -> getPluginDesc pluginName plugin
        sendMessage channelId . msgEmbed $
            mconcat
            [ embedTitle "Available plugins"
            , embedDesc "Type `$help <plugin> [subcommands]` to get help for a specific plugin."
            , desc
            ]
        return ()

    handler _ Message {..} (Help name args) = do
        plugs <- asks (botPlugins . botSettings)
        prefix <- getCommandPrefix
        let target = find ((== name) . pluginName) plugs
            result = target >>= \WrappedPlugin {..} -> getPluginHelp (prefix <> pluginName) (args ++ ["--help"]) plugin
        mapM_ (sendMessage channelId . msgText . codeBlock) result

    getPluginHelp :: Text -> [Text] -> Plugin name opcode event s -> Maybe Text
    getPluginHelp name args (CmdPlugin _ (CommandHandler _ parser)) =
        case execParserPure defaultPrefs parser (map unpack args) of
            Failure f ->
                let (h, _, _) = execFailure f (unpack name)
                in return $ pack $ renderHelp 80 h
            _ -> Nothing
    getPluginHelp _ _ _ = Nothing

    getPluginDesc :: Text -> Plugin name opcode event s -> Embed
    getPluginDesc name (CmdPlugin _ (CommandHandler _ parser)) =
        embedField name (maybe "No description available." (pack . show) (unChunk (infoProgDesc parser)))
    getPluginDesc _ _ = mempty


