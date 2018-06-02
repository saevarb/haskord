module Haskord.Sandbox where


import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Concurrent
import Data.Text (pack)

import Haskord.Types

sandbox :: Int -> BotM () -> BotM ()
sandbox duration fn = do
    s <- ask
    void . liftIO . async $ runBotM s $ do
        sandboxId <- liftIO $ async $ void $ runBotM s fn
        killerId <- liftIO $ async $ threadDelay (duration * 1000000) >> cancel sandboxId
        (_, res) <- liftIO $ waitAnyCatchCancel [sandboxId, killerId]
        case res of
            Left e -> logE' "Sandboxed thread crashed" e
            Right _ -> return ()
