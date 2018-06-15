{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Haskord.Plugins.Resources
    ( resourcePlugin
    ) where

import           Control.Monad
import           Data.Bool
import           Data.Functor
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Void

import           Database.Persist        as SQL
import           Database.Persist.Sqlite as SQL
import           Database.Persist.TH     as SQL
import           Network.URI
import           Text.Megaparsec         as M
import           Text.Megaparsec.Char    as M

import           Haskord.Http
import           Haskord.Types


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Resource
    title Text
    link Text
    deriving Eq Show
Tag
    name Text
    UniqueTag name
    deriving Eq Show
TagRelation
    resource ResourceId
    tag TagId
    deriving Eq Show
|]


type Parser = Parsec Void Text


data ResourceCommand
    = AddResource Text Text [Text]
    | ListTags
    | Search [Text]
    deriving (Eq, Show)

resourceCommandP :: Parser ResourceCommand
resourceCommandP =
    (string "resources" <|> string "resource") >> space1 *> choice parsers <* eof
  where
    parsers =
        [ addP
        , listTagsP
        , searchP
        ]
    addP = do
        void $ string "add"
        space1 <?> "space after add"
        title <- titleP
        space1 <?> "space after title"
        link <- linkP
        space <?> "space after link"
        tags <- tagListP <?> "list of tags"
        return $ AddResource title link tags
    searchP = do
        void $ string "search"
        space1
        Search <$> tagListP
    listTagsP = string "list-tags" $> ListTags
    titleP = T.pack <$> between "["  "]" (some (try alphaNumChar <|> spaceChar))
    linkP = urlP <?> "valid url"
    tagP = T.pack <$> some (try alphaNumChar <|> oneOf ("-_" :: String))
    tagListP = tagP `sepBy1` space1


urlP :: Parser Text
urlP = do
    rawUrl <- someTill (oneOf urlChars) (noneOf urlChars)
    case parseURI rawUrl of
        Just _  -> return $ T.pack rawUrl
        Nothing -> fail "Expected valid url"
  where
    urlChars =
        ['A' .. 'Z']
        ++ ['a' .. 'z']
        ++ ['0' .. '9']
        ++ "-._~:/?#[]@!$&'()*+,;=%"


resourcePlugin :: DispatchPlugin "Resources plugin" 'MESSAGE_CREATE ()
resourcePlugin =
    Plugin
    { initializePlugin = runDb $ runMigration migrateAll
    , runPlugin = const handler
    }
  where
    handler :: DispatchPayload 'MESSAGE_CREATE -> BotM ()
    handler (MessageCreatePayload msg@Message {..}) = do
        let isMe u = username u == "Haskord"
        unless (username author == "Haskord") $ when (any isMe mentions) $ do
            let fixedContent = T.unwords . drop 1 . T.words $ content
            logI' "Fixed content" fixedContent
            case M.parse resourceCommandP "ResourceCommand" fixedContent of
                Right cmd ->
                    commandHandler msg cmd
                Left _ ->
                    return ()

    commandHandler :: Message -> ResourceCommand -> BotM ()
    commandHandler Message {..} (AddResource title link tags) =
        if null tags then
            void $ sendMessage channelId $ msgText "You forgot the tags"
            else do
            newTagNames <- runDb $ do
                newTagIds <- mapM (SQL.insertUnique . Tag) tags
                newTagNames <- mapM SQL.get $ catMaybes newTagIds
                return . map tagName $ catMaybes newTagNames
            newResId <- runDb $ do
                allTags <- mapM (getBy . UniqueTag) tags
                newResource <- insert (Resource title link)
                void $ insertMany $ map (TagRelation newResource . entityKey) $ catMaybes allTags
                return newResource
            let tagMsg =
                    bool mempty ("Created new tags: " <> T.intercalate ", " newTagNames) (not $ null newTagNames)
            void $ sendMessage channelId $ msgText $
                T.unlines
                [ tagMsg
                , "Resource added. ID: **" <> (T.pack . show $ fromSqlKey newResId) <> "**"
                ]

    commandHandler Message {..} ListTags = do
        tags <- runDb $ do
            tags <- selectList [] []
            return $ map (tagName . entityVal) tags
        void $ sendMessage channelId $
            msgText "I know these tags: "
            <> msgText (T.intercalate ", " tags)

    commandHandler Message {..} (Search tags) = unless (null tags) $ do
        let tagFilters = foldl1 (||.) $
                map (pure . (TagName ==.)) tags
        ts <- runDb $ selectKeysList tagFilters []
        logI' "Tags" ts
        let relationFilters = foldl1 (||.) $
                map (pure . (TagRelationTag ==.)) ts
        relations <- runDb $ selectList relationFilters []
        logI' "Relations" relations
        -- guard (not $ null relations)
        let resourceFilters = foldl1 (||.) $
                map (pure . (ResourceId ==.) . tagRelationResource . entityVal) relations
        resources <- runDb $ selectList resourceFilters [Desc ResourceId]
        logI' "Resources" resources
        let resourceMsgs = do
                resEnt <- resources
                let resKey = T.pack . show $ fromSqlKey (entityKey resEnt)
                    res = entityVal resEnt
                return $ "**" <> resKey <> "** - " <> resourceTitle res <> " - " <> resourceLink res
        void $ sendMessage channelId $
            msgText $ T.unlines
            [ "Resources tagged with " <> T.intercalate ", " tags
            , T.unlines resourceMsgs
            ]
