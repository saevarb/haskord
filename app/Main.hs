module Main where

import Haskord
import Plugins.Resources

main :: IO ()
main = do
    runBotWithSettings "config.yaml" $
        defaultSettings
        # withPlugin (runnablePlugin resourcePlugin)
