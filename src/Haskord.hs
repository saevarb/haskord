module Haskord where

import           Control.Concurrent            (threadDelay)
import           Control.Monad
import           Control.Monad.State
import GHC.Stack

import           Brick.BChan
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Safe
import qualified Control.Monad.Logger          as ML
import           Data.Aeson
import qualified Data.Yaml                     as Y
import qualified Database.Persist.Sqlite       as SQL
import           Network.WebSockets.Connection
import           Streaming                     as S
import qualified Streaming.Prelude             as S
import           Text.Pretty.Simple
import           Wuss

import           Haskord.Config
import           Haskord.Http
import           Haskord.Plugins
import           Haskord.Plugins.Default
import           Haskord.Plugins.Resources
import           Haskord.Rendering
import           Haskord.WebSocket


plugins :: [RunnablePlugin]
plugins =
    concat
    [ defaultPlugins
    , [runnablePlugin resourcePlugin]
    ]


initializeBotState :: BotConfig -> IO BotState
initializeBotState cfg = do
    gateway         <- getGateway (botToken cfg)
    seqVar          <- newEmptyTMVarIO
    sessionVar      <- newEmptyTMVarIO
    htidvar         <- newEmptyTMVarIO
    writerThreadVar <- newEmptyTMVarIO
    meVar           <- newEmptyTMVarIO
    gatewayQueue    <- newTQueueIO
    eventChan       <- newBChan 1000
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
            , writerThreadId = writerThreadVar
            , dbConnPool = connPool
            , gatewayUrl = drop 6 . unpack . gwUrl $ gateway
            , me = meVar
            }
  where
    makeLogger chan event title msg =
        liftIO $ writeBChan chan $ event (title, msg)

startPipeline :: Connection -> BotState -> IO (Async ())
startPipeline conn botState =
    async $ void $ flip runStateT botState $ runBotM $ do
        initializePlugins plugins
        S.mapM_ (runPlugins plugins)
            $ logPayloads
            $ updateSequenceNumber $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map eitherDecode
            $ wsSource conn
  where
    logPayloads :: Stream (Of SomeMessage) BotM r -> Stream (Of SomeMessage) BotM r
    logPayloads =
        S.chain $ \(SomeMessage _ p) -> logI "Payload" (pack $ show p)

    updateSequenceNumber :: Stream (Of SomeMessage) BotM r -> Stream (Of SomeMessage) BotM r
    updateSequenceNumber =
        S.chain (updateSeqNo . seqNo)


app :: HasCallStack => BotState -> Connection -> IO ()
app botState conn = do
    putStrLn "starting app.."
    writerId <- startWriterThread (gwQueue botState) conn
    empty' <- atomically $ isEmptyTMVar (writerThreadId botState)
    if empty' then
        void $ atomically $ putTMVar (writerThreadId botState) writerId
        else
        void $ atomically $ swapTMVar (writerThreadId botState) writerId
    tid <- startPipeline conn botState
    -- link tid
    -- crashThread <- async $ threadDelay (5 * 10^6) >> fail "Intentional error"
    -- link crashThread
    renderInterface (eventChan botState)
    threadDelay (5 * 10^6) >> do
        cancel tid
        fail "What"
    return ()

resumeBot :: HasCallStack => BotState -> IO ()
resumeBot botState@BotState {..} = do
    -- putStrLn "Resuming bot.."
    writerTid <- atomically $ takeTMVar writerThreadId
    heartbeatTid <- atomically $ takeTMVar heartbeatThreadId
    uninterruptibleCancel writerTid
    uninterruptibleCancel heartbeatTid
    cancel writerTid
    cancel heartbeatTid
    -- putStrLn "Reading session var"
    sessId <- atomically $ readTMVar sessionIdVar
    -- putStrLn "Reading seq  var"
    seqNo <- atomically $ readTMVar seqNoVar
    -- putStrLn "Sending resume command.."
    let cmd = ResumeCmd (Resume' (botToken botConfig) sessId seqNo)
    atomically $ writeTQueue gwQueue cmd
    print (encode cmd)
    startClientWithState botState
    return ()


handleException :: HasCallStack => BotState -> SomeException -> IO ()
handleException botState e = do
    putStrLn "Crashed! Resuming..: "
    pPrint e
    resumeBot botState

startClientWithState :: HasCallStack => BotState -> IO ()
startClientWithState botState =
    runSecureClient (gatewayUrl botState) 443 "/?v=6&&encoding=json" (app botState)
        `catch`
        handleException botState

startClientWithConfig :: HasCallStack => BotConfig -> IO ()
startClientWithConfig cfg = do
    botState <- initializeBotState cfg
    startClientWithState botState

runBotWithConfig :: HasCallStack => FilePath -> IO ()
runBotWithConfig cfgFile = do
    cfg <- readConfig cfgFile
    case cfg of
        Left ex   -> putStrLn $ Y.prettyPrintParseException ex
        Right cfg' -> startClientWithConfig cfg'
