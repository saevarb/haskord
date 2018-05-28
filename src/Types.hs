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

module Types where

import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString.Lazy     (ByteString)
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                (Text, pack, unpack)
import qualified Data.Vector              as V
import           Data.Word                (Word64)
import           GHC.Generics

import           Brick.BChan
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Types

import           Config
import           Rendering
import           Types.Gateway


data BotState
    = BotState
    { sessionIdVar      :: TMVar String
    , seqNoVar          :: TMVar Int
    , heartbeatThreadId :: TMVar (Async ())
    , botConfig         :: BotConfig
    , gwQueue           :: TQueue GatewayCommand
    , logInfo           :: Text -> Text -> IO ()
    , logErr            :: Text -> Text -> IO ()
    , eventChan         :: BChan RenderEvent
    }

newtype BotM a
    = BotM
    { runBotM :: StateT BotState IO a
    } deriving (Applicative, Monad, MonadIO, MonadState BotState, Functor)


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


