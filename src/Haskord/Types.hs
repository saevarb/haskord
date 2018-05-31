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


data BotState
    = BotState
    { sessionIdVar      :: TMVar Text
    , seqNoVar          :: TMVar Int
    , heartbeatThreadId :: TMVar (Async ())
    , writerThreadId    :: TMVar (Async ())
    , me                :: TMVar User
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

logI :: Text -> Text -> BotM ()
logI title msg = do
    li <- gets logInfo
    liftIO $ li title msg

logE :: Text -> Text -> BotM ()
logE title msg = do
    li <- gets logErr
    liftIO $ li title msg


toGateway :: GatewayCommand -> BotM ()
toGateway x = do
    q <- gets gwQueue
    liftIO . atomically $ writeTQueue q x


