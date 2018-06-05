module Main where

import           Haskord
import           Plugins.Eval
import           Plugins.Resources

main :: IO ()
main = do
    runBotWithSettings "config.yaml" $
        defaultSettings
        # withPlugin (wrapPlugin resourcePlugin)
        # withPlugin (wrapPlugin evalPlugin)
