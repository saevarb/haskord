{-# LANGUAGE DisambiguateRecordFields #-}
module Haskord.Plugins.Default
    ( defaultPlugins
    ) where

import           Control.Monad
import           Control.Monad.Reader.Class

import           Streaming                  as S
import qualified Streaming.Prelude          as S

import           Haskord.Prelude
import           Haskord.Types

defaultPlugins :: [WrappedPlugin]
defaultPlugins =
    [ wrapPlugin helloPlugin
    , wrapPlugin readyPlugin
    , wrapPlugin chatLoggerPlugin
    ]

chatLoggerPlugin :: DispatchPlugin 'MESSAGE_CREATE ()
chatLoggerPlugin =
    simplePlugin $ \(MessageCreatePayload Message {..}) ->
        logI' ("Message from " <> username author) content

readyPlugin :: DispatchPlugin 'READY ()
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

helloPlugin :: RawPlugin 'Hello ()
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


