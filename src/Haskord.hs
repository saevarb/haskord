module Haskord
    ( defaultSettings
    , withPlugin
    , withPlugins
    , runnablePlugin
    , (#)
    , runBotWithSettings
    ) where

import           GHC.Stack

import           Control.Exception.Safe
import qualified Control.Monad.Logger          as ML
import           Data.Aeson
import           Data.Singletons
import qualified Data.Yaml                     as Y
import qualified Database.Persist.Sqlite       as SQL
import           Network.WebSockets.Connection
import           Streaming                     as S
import qualified Streaming.Prelude             as S
import           Text.Pretty.Simple
import           Wuss
import Haxl.Core.DataCache

import           Haskord.Http
import           Haskord.Logging               as L
import           Haskord.Plugins.Default
import           Haskord.Prelude
import           Haskord.Rendering
import           Haskord.Types
import           Haskord.WebSocket

initializeBotState :: BotSettings -> IO BotState
initializeBotState settings@BotSettings {..} = do
    gateway         <- getGateway (botToken botConfig)
    seqVar          <- newEmptyTMVarIO
    sessionVar      <- newEmptyTMVarIO
    htidvar         <- newEmptyTMVarIO
    writerThreadVar <- newEmptyTMVarIO
    meVar           <- newEmptyTMVarIO
    gatewayQueue    <- newTQueueIO
    logV            <- newTVarIO (L.empty 1000)
    eventChan       <- newBChan 1000
    connPool        <- ML.runStderrLoggingT $ SQL.createSqlitePool "db.sqlite" 10
    cacheVar        <- newTVarIO emptyDataCache
    return BotState
            { sessionIdVar      = sessionVar
            , seqNoVar          = seqVar
            , botSettings       = settings
            , gwQueue           = gatewayQueue
            , logVar = logV
            , eventChan         = eventChan
            , heartbeatThreadId = htidvar
            , writerThreadId    = writerThreadVar
            , dbConnPool        = connPool
            , gatewayUrl        = drop 6 . unpack . gwUrl $ gateway
            , me                = meVar
            , requestCache      = cacheVar
            }

startPipeline :: Connection -> BotState -> IO (Async ())
startPipeline conn botState =
    async $ void $ runBotM botState $ do
        initializePlugins (botPlugins$ botSettings botState)
        S.mapM_ (runPlugins (botPlugins $ botSettings botState))
            $ logPayloads
            $ updateSequenceNumber $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map eitherDecode
            $ wsSource conn
  where
    logPayloads :: Stream (Of SomeMessage) BotM r -> Stream (Of SomeMessage) BotM r
    logPayloads =
        S.chain $ \(SomeMessage _ sev sop p) ->
                      logI' (pack $ show (fromSing sop) <> " - " <> show (fromSing sev)) p

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
    renderInterface (logVar botState) (eventChan botState)
    -- threadDelay (5 * 10^6) >> do
    --     cancel tid
    --     fail "What"
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
    sessId <- atomically $ readTMVar sessionIdVar
    seqNo <- atomically $ readTMVar seqNoVar
    let cmd = ResumeCmd (Resume' (botToken . botConfig $ botSettings) sessId seqNo)
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

startClientWithSettings :: HasCallStack => BotSettings -> IO ()
startClientWithSettings settings = do
    botState <- initializeBotState settings
    startClientWithState botState

runBotWithSettings :: HasCallStack => FilePath -> BotSettings -> IO ()
runBotWithSettings cfgFile settings = do
    cfg <- readConfig cfgFile
    case cfg of
        Left ex   -> putStrLn $ Y.prettyPrintParseException ex
        Right cfg' ->
            startClientWithSettings $ settings { botConfig = cfg' }

defaultSettings :: BotSettings
defaultSettings =
    BotSettings
    { botPlugins = defaultPlugins
    }
