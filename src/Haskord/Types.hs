{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Haskord.Types where

import           Control.Applicative
import           Control.Monad.State

import           Brick.BChan
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Data.Pool
import           Database.Persist.Sqlite

import           Haskord.Config
import           Haskord.Rendering
import           Haskord.Types.Common
import           Haskord.Types.Gateway
import Haskord.Logging as L


data BotState
    = BotState
    { sessionIdVar      :: TMVar Text
    , seqNoVar          :: TMVar Int
    , heartbeatThreadId :: TMVar (Async ())
    , writerThreadId    :: TMVar (Async ())
    , me                :: TMVar User
    , logVar            :: TVar (BoundedLog LogMessage)
    , botConfig         :: BotConfig
    , gwQueue           :: TQueue GatewayCommand
    , logInfo           :: Text -> Text -> IO ()
    , logErr            :: Text -> Text -> IO ()
    , eventChan         :: BChan RenderEvent
    , dbConnPool        :: Pool SqlBackend
    , gatewayUrl        :: String
    }

newtype BotM a
    = BotM
    { runBotM :: StateT BotState IO a
    } deriving (Applicative, Monad, MonadIO, MonadState BotState, Functor)


runDb :: SqlPersistT IO a -> BotM a
runDb query = do
    pool <- gets dbConnPool
    liftIO $ runSqlPool query pool

makeLogger
  :: (MonadState BotState m, MonadIO m) => Severity -> Text -> m ()
makeLogger s e = do
    lv <- gets logVar
    liftIO $ atomically $ modifyTVar lv (L.insert (LogMessage s e (Nothing :: Maybe ())))

makeLogger'
  :: (MonadState BotState m, MonadIO m) => Show p => Severity -> Text -> p -> m ()
makeLogger' s e p = do
    lv <- gets logVar
    liftIO $ atomically $ modifyTVar lv (L.insert (LogMessage s e (Just p)))


logI, logW, logE, logF :: (MonadState BotState m, MonadIO m) => Text -> m ()
logI = makeLogger Info
logW = makeLogger Warning
logE = makeLogger Error
logF = makeLogger Fatal

logI', logW', logE', logF' :: (MonadState BotState m, MonadIO m, Show p) => Text -> p -> m ()
logI' = makeLogger' Info
logW' = makeLogger' Warning
logE' = makeLogger' Error
logF' = makeLogger' Fatal



toGateway :: GatewayCommand -> BotM ()
toGateway x = do
    q <- gets gwQueue
    liftIO . atomically $ writeTQueue q x


