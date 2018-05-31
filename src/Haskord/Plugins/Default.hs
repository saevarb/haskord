{-# LANGUAGE DisambiguateRecordFields #-}
module Haskord.Plugins.Default where

import Control.Monad
import Control.Monad.State.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Text (pack)

import           Streaming              as S
import qualified Streaming.Prelude      as S

import Haskord.Plugins

import Haskord.Config

defaultPlugins :: [RunnablePlugin]
defaultPlugins =
    [ runnablePlugin helloPlugin
    , runnablePlugin readyPlugin
    , runnablePlugin chatLoggerPlugin
    ]

chatLoggerPlugin :: DispatchPlugin 'MESSAGE_CREATE ()
chatLoggerPlugin =
    simplePlugin $ \(MessageCreatePayload (Message {..})) ->
        logI ("Message from " <> username author) content

readyPlugin :: DispatchPlugin 'READY ()
readyPlugin =
    simplePlugin $ \(ReadyPayload ready) -> do
       sessVar <- gets sessionIdVar
       meVar <- gets me
       liftIO . atomically $ putTMVar meVar (user_  ready)
       liftIO $ atomically $ do
           isEmpty <- isEmptyTMVar sessVar
           if isEmpty then
               void $ putTMVar sessVar (sessionId ready)
               else
               void $ swapTMVar sessVar (sessionId ready)
       logI "Ready received" (pack $ show ready)

helloPlugin :: RawPlugin 'Hello ()
helloPlugin =
    simplePlugin readyHandler
  where
    readyHandler :: RawPayload 'Hello -> BotM ()
    readyHandler (HelloPayload hello) = do
        token <- gets (botToken . botConfig)
        meVar <- gets me
        notIdentified <- liftIO . atomically $ isEmptyTMVar meVar
        when notIdentified $
            toGateway $ IdentifyCmd $ identPayload token
        startHeartbeatThread $ heartbeatInterval hello

    startHeartbeatThread :: Int -> BotM ()
    startHeartbeatThread interval = do
        logI "Starting heartbeat thread.." "Yep"
        gwq <- gets gwQueue
        htidvar <- gets heartbeatThreadId
        htid <- liftIO $ atomically $ tryTakeTMVar htidvar
        case htid of
            Just n -> liftIO $ cancel n
            Nothing -> return ()
        logger <- gets logInfo
        tid <- liftIO $ async $
            queueSink gwq
            $ S.chain (\_ -> liftIO $ logger "Sending heartbeat..." "")
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


