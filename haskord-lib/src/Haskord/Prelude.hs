module Haskord.Prelude
    ( module Data.Monoid
    , module Data.Aeson
    , module Control.Monad.Reader
    , module Control.Applicative
    , module Control.Concurrent.Async
    , module Control.Concurrent.STM
    , Text
    , JSONParser
    , TL.toStrict
    , smartWords
    ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Aeson               (FromJSON (..), Value, withObject,
                                           (.:))
import           Data.Aeson.Types         (Parser)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Lazy           as TL
import Data.Char
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe (fromMaybe)


type JSONParser         = Data.Aeson.Types.Parser

type Parser' = Parsec Void Text

-- |Splits `Text` like `words` but does "shell-like" splitting such that anything enclosed in
-- quotes("") or brackets ([]) is considered a single word. Mismatched delimiters are ignored
-- smartWords "foo bar \"baz qux\" xyz" = ["foo", "bar", "baz qux", "xyz"]
-- smartWords :: Text -> [Text]
smartWords :: Text -> [Text]
smartWords =
    fromMaybe [] . parseMaybe (some (try quoteWord <|> spaceWord) <* eof)
  where
    spaceWord :: Parser' Text
    spaceWord =
        space *> takeWhile1P (Just "space word") (not . isSpace) <* space
    quoteWord :: Parser' Text
    quoteWord =
        char '"' *> takeWhile1P (Just "quote word") (/= '"') <* char '"'
        -- (T.pack <$> between (char '"') (char '"') (some anyChar)) <* space

