module Haskord.Sandbox where


import Control.Monad
import Control.Monad.State
import Control.Concurrent.Async
import Control.Concurrent
import Data.Text (pack)

import Haskord.Types

sandbox :: Int -> BotM () -> BotM ()
sandbox duration fn = do
    s <- get
    -- logger <- gets logInfo
    void . liftIO . async $ do
        sandboxId <- async $ void $ flip runStateT s $ runBotM fn
        killerId <- async $ threadDelay (duration * 1000000) >> cancel sandboxId
        (_, res) <- waitAnyCatchCancel [sandboxId, killerId]
        case res of
            Left e -> return ()
            Right _ -> return ()
