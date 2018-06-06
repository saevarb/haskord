{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import qualified Data.Text as T

import Haskord
import Haskord.Types


reversePlugin
    :: DispatchPlugin    -- we want to handle a dispatch message
       "Reverse plugin"  -- the plugin's name
       'MESSAGE_CREATE   -- the dispatch event is MESSAGE_CREATE
       ()                -- No state
reversePlugin =
    -- simplePlugin assumes you don't want any state, the handler function
    -- only takes the message payload as parameter
    simplePlugin $ \(MessageCreatePayload Message {..}) -> do
        let split = T.words content
        case split of
            ("!reverse":rest) ->
                void $ sendMessage channelId . msgText . T.reverse $ T.unwords rest
            _ -> return ()
        return ()

counterPlugin
    :: DispatchPlugin    -- we want to handle a dispatch message
       "Counter plugin"  -- the plugin's name
       'MESSAGE_CREATE   -- the dispatch event is MESSAGE_CREATE
       Int               -- We have one integer as state
counterPlugin =
    -- For a plugin that uses state, we define `initializePlugin` to give us the initial state
    Plugin
    { initializePlugin = return 0
    , runPlugin        = counterHandler
    }
  where
    -- Note that this handler takes `TVar Int` because our plugin's state is `Int`
    counterHandler :: TVar Int -> DispatchPayload 'MESSAGE_CREATE -> BotM ()
    counterHandler stateVar (MessageCreatePayload Message {..}) = do
        let split = T.words content
        case split of
            ("!count":_) -> do
                counter <- liftIO $ atomically $ do
                    modifyTVar' stateVar succ
                    readTVar stateVar
                void $ sendMessage channelId . msgText $ "Counter: " <> T.pack (show counter)
            _ -> return ()
        return ()

main :: IO ()
main =
    -- config.yaml should be in the working directory
    runBotWithSettings "config.yaml" $
      defaultSettings
      -- We need to "homogenize" the plugins since they have different types, so we call `wrapPlugin`
      -- on each, individually. `map wrapPlugin [counterPlugin, reversePlugin]` does not work.
      # withPlugins
        [ wrapPlugin counterPlugin
        , wrapPlugin reversePlugin
        ]
