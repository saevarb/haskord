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
import           Control.Monad.Reader

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
    , eventChan         :: BChan RenderEvent
    , dbConnPool        :: Pool SqlBackend
    , gatewayUrl        :: String
    }

newtype BotM a
    = BotM
    { unBotM :: ReaderT BotState IO a
    } deriving (Applicative, Monad, MonadIO, MonadReader BotState, Functor)

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


