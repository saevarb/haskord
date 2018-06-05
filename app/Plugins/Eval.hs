module Plugins.Eval where

import qualified Data.Text as T
import Control.Concurrent.QSem
import Control.Monad

import Control.Concurrent.STM.TVar
import Language.Haskell.Interpreter
import Control.Exception.Safe

import Haskord.Types

evalPlugin :: DispatchPlugin "Haskell eval plugin" 'MESSAGE_CREATE QSem
evalPlugin =
    Plugin
    { initializePlugin = liftIO $ newQSem 1
    , runPlugin = evalHandler
    }

evalHandler :: TVar QSem -> DispatchPayload 'MESSAGE_CREATE -> BotM ()
evalHandler qvar (MessageCreatePayload Message {..}) = when (username author /= "Haskord") $ do
    qsem <- liftIO $ readTVarIO qvar
    let split = T.words content
    logI' "Eval plugin" content
    case split of
        (">>>":rest) -> do
            res <- liftIO $ bracket_ (waitQSem qsem) (signalQSem qsem) $ do
                runInterpreter $ initializeInterpreter (T.unpack $ T.unwords rest)
            logI' "Sending eval reply" res
            sendMessage channelId $
                    msgText $ T.pack $ either show Prelude.id res
            return ()
        _ -> logI "Eval plugin didn't run"

initializeInterpreter :: MonadInterpreter m => String -> m String
initializeInterpreter str = do
    reset
    set [languageExtensions := extensions]
    setImportsQ imports
    eval str
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
      [ AutoDeriveTypeable
      , BangPatterns
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
      , UnknownExtension "EmptyDataDeriving"
      , ExistentialQuantification
      , ExplicitForAll
      , ExplicitNamespaces
      , ExtendedDefaultRules
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
      , PostfixOperators
      , Rank2Types
      , RankNTypes
      , RebindableSyntax
      , RecordPuns
      , RecordWildCards
      , RecursiveDo
      , UnknownExtension "RelaxedLayout"
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
      , UnknownExtension "UnboxedSums"
      , ViewPatterns
      ]
