module Haskord where

import           Control.Concurrent     hiding (throwTo)
import           Control.Monad
import           Control.Monad.State

import           Brick.BChan
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml              as Y
import           Streaming              as S
import qualified Streaming.Prelude      as S
import           Text.Pretty.Simple
import           Wuss
import qualified Database.Persist.Sqlite as SQL
import qualified Control.Monad.Logger as ML
import Control.Concurrent.Async
import Network.WebSockets.Connection

import           Haskord.Config
import           Haskord.Http
import           Haskord.Plugins
import           Haskord.Plugins.Default
import           Haskord.Plugins.Resources
import           Haskord.Rendering
import           Haskord.Types
import           Haskord.Types.Common
import           Haskord.Types.Gateway
import Haskord.WebSocket


plugins :: [RunnablePlugin]
plugins =
    concat
    [ defaultPlugins
    , [runnablePlugin resourcePlugin]
    ]


initializeBotState :: BotConfig -> Connection -> IO BotState
initializeBotState cfg conn = do
    seqVar       <- newEmptyTMVarIO
    sessionVar   <- newEmptyTMVarIO
    htidvar      <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    eventChan    <- newBChan 1000
    startWriterThread gatewayQueue conn
    connPool <- ML.runStderrLoggingT $ SQL.createSqlitePool "db.sqlite" 10
    return BotState
            { sessionIdVar = sessionVar
            , seqNoVar     = seqVar
            , botConfig    = cfg
            , gwQueue      = gatewayQueue
            , logInfo      = makeLogger eventChan MessageAdded
            , logErr       = makeLogger eventChan ErrorAdded
            , eventChan    = eventChan
            , heartbeatThreadId = htidvar
            , dbConnPool = connPool
            }
  where
    makeLogger chan event title msg =
        liftIO $ writeBChan chan $ event (title, msg)

app :: BotConfig -> Connection -> IO ()
app cfg conn = do
    botState <- initializeBotState cfg conn
    tid <- async $ void $ flip runStateT botState $ runBotM $ do
        initializePlugins plugins
        S.mapM_ (runPlugins plugins)
            $ logPayloads
            $ updateSequenceNumber $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map eitherDecode
            $ wsSource conn
    link tid
    renderInterface (eventChan botState)
    cancel tid
    return ()
  where
    logPayloads =
        S.chain $ \(SomeMessage _ p) -> logI "Payload" (pack $ show p)
    updateSequenceNumber =
        S.chain (updateSeqNo . seqNo)

handleException :: SomeException -> IO ()
handleException e = do
    putStrLn "Oops!"
    pPrint e

runBotWithConfig :: FilePath -> IO ()
runBotWithConfig cfgFile = do
    cfg <- readConfig cfgFile
    case cfg of
        Left ex -> do
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ gwUrl gateway) 443 "/?v=6&&encoding=json" (app cfg) `catch` handleException
