module Haskord.Plugins.Eval where

import qualified Data.Text as T
import Control.Monad
import System.Process
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Concurrent.Async
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Bifunctor (first)
import Control.Exception.Safe

import Language.Haskell.Interpreter

import Haskord.Types hiding (some, Parser)

data Command
    = Eval Text
    | TypeOf Text
    | KindOf Text
    deriving (Show, Eq)

type Parser = Parsec Void Text


evalPlugin :: DispatchPlugin "Haskell eval plugin" 'MESSAGE_CREATE QSem
evalPlugin =
    Plugin
    { pInitializer = liftIO (newQSem 1)
    , pHandler = evalHandler
    }

evalHandler :: TVar QSem -> DispatchPayload 'MESSAGE_CREATE -> BotM ()
evalHandler qvar (MessageCreatePayload Message {..}) = when (username author /= "Haskord") $ do
    let res = parse commandP "haskell eval" content
    case res of
        Right cmd -> do
            qsem <- liftIO $ readTVarIO qvar
            createReaction channelId id_ "👀"
            message <- either ("Error: " <>) Prelude.id <$> evalCommand qsem cmd
            void $ sendMessage channelId . msgText $ T.pack $ unlines ["```", message, "```"]
            createReaction channelId id_ "☑"
        Left _ -> return ()

evalCommand :: QSem -> Command -> BotM (Either String String)
evalCommand _ (Eval expr)   =
    runMueval (unpack expr)
evalCommand qsem (TypeOf expr) =
    bracket_ (liftIO $ waitQSem qsem)  (liftIO $ signalQSem qsem) $ first ppError <$> getType (unpack expr)
evalCommand qsem (KindOf expr) =
    bracket_ (liftIO $ waitQSem qsem)  (liftIO $ signalQSem qsem) $ first ppError <$> getKind (unpack expr)


runMueval :: String -> BotM (Either String String)
runMueval expr = do
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" args ""
    logI' "mueval exit code" (ec, out, err)
    case ec of
        ExitSuccess -> return $ Right $ out <> err
        _ -> return $ Left $ out <> err
  where
    timeout :: Int
    timeout = 15
    args = ["exec", "--", "mueval-core", "-t", show timeout, "--expression", expr, "+RTS", "-N2", "+RTS"]

getType :: String -> BotM (Either InterpreterError String)
getType =
    liftIO . runInterpreter . runHint typeOf

getKind :: String -> BotM (Either InterpreterError String)
getKind =
    liftIO . runInterpreter . runHint kindOf

ppError :: InterpreterError -> String
ppError (WontCompile errors) = unlines $ map errMsg errors
ppError _ = "Unknown error. Summon the pope for an exorcism!"


commandP :: Parser Command
commandP = choice [evalP, typeOfP, kindOfP]
  where
    evalP :: Parser Command
    evalP = string ">" >> Eval . pack <$> (space *> some anyChar <* eof)
    typeOfP = string ":t" >> TypeOf . pack <$> (space1 *> some anyChar <* eof)
    kindOfP = string ":k" >> KindOf . pack <$> (space1 *> some anyChar <* eof)

runHint :: MonadInterpreter m => (String -> m String) -> String -> m String
runHint fn str = do
    reset
    set [languageExtensions := extensions]
    setImportsQ imports
    fn str
  where
    imports =
        [ ("Prelude", Nothing)
        , ("Data.List", Nothing)
        , ("Data.Foldable", Nothing)
        , ("Control.Monad", Nothing)
        , ("Data.String", Nothing)
        -- , ("Plugins.EvalUtil", Nothing)
        ]

    extensions =
      [ BangPatterns
      , BinaryLiterals
      , ConstraintKinds
      , DataKinds
      , DefaultSignatures
      , DeriveDataTypeable
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , DisambiguateRecordFields
      , DuplicateRecordFields
      , EmptyCase
      , EmptyDataDecls
      , ExistentialQuantification
      , FlexibleContexts
      , FlexibleInstances
      , FunctionalDependencies
      , GADTs
      , GeneralizedNewtypeDeriving
      , ImplicitPrelude
      , TypeFamilyDependencies
      , InstanceSigs
      , ApplicativeDo
      , KindSignatures
      , LambdaCase
      , MonadComprehensions
      , MonomorphismRestriction
      , MultiParamTypeClasses
      , MultiWayIf
      , NPlusKPatterns
      , NamedFieldPuns
      , NamedWildCards
      , NegativeLiterals
      , NondecreasingIndentation
      , NullaryTypeClasses
      , NumDecimals
      , OverlappingInstances
      , OverloadedLabels
      , OverloadedStrings
      , PartialTypeSignatures
      , PatternGuards
      , PatternSignatures
      , PatternSynonyms
      , PolyKinds
      , PolymorphicComponents
      , Rank2Types
      , RankNTypes
      , RoleAnnotations
      , ScopedTypeVariables
      , StandaloneDeriving
      , TraditionalRecordSyntax
      , TransformListComp
      , TupleSections
      , TypeApplications
      , TypeFamilies
      , TypeOperators
      , TypeSynonymInstances
      , UnboxedTuples
      , ViewPatterns
      ]
