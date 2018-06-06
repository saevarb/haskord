module Main where

import           Haskord
import           Haskord.Plugins.Eval
import           Haskord.Plugins.Resources

main :: IO ()
main = do
    runBotWithSettings "config.yaml" $
        defaultSettings
        # withPlugin (wrapPlugin resourcePlugin)
        # withPlugin (wrapPlugin evalPlugin)
