module Haskord.Prelude
    ( Text
    , TL.toStrict
    , JSONParser
    , module Data.Monoid
    , module Data.Aeson
    , module Control.Monad.Reader
    , module Control.Applicative
    , module Control.Concurrent.Async
    , module Control.Concurrent.STM
    ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Aeson               (FromJSON (..), Value, withObject,
                                           (.:))
import           Data.Aeson.Types         (Parser)
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy           as TL


type JSONParser         = Data.Aeson.Types.Parser

