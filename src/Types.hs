{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.Text            (Text, pack, unpack)
import           Data.Word            (Word64)
import           GHC.Generics
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Data.Proxy

import           Data.Aeson
import           Data.Aeson.Types
import           Control.Concurrent.STM

import Config
import Types.Gateway

data BotState
    = BotState
    { sessionIdVar :: TMVar String
    , seqNoVar     :: TMVar Int
    , botConfig    :: BotConfig
    , gwQueue      :: TQueue GatewayCommand
    }

newtype BotM a
    = BotM
    { runBotM :: StateT BotState IO a
    } deriving (Applicative, Monad, MonadIO, MonadState BotState, Functor)


-- class (FromJSON (Type a)) => HasType a where
--     type Type (a :: EventType) :: *

--     decode'' :: Proxy a -> ByteString -> Maybe (Type a)
--     default decode'' :: Proxy a -> ByteString -> Maybe (Type a)
--     decode'' v = decode


--     -- | MESSAGE_REACTION_REMOVE
-- instance HasType 'MESSAGE_REACTION_ADD where
--     type Type 'MESSAGE_REACTION_ADD = Reaction



