{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Concurrent     hiding (throwTo)
import           Control.Exception      (throwIO)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.ByteString.Lazy   as B
import           Data.Monoid
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL (toStrict, unlines, unpack)
import qualified Data.Text.Lazy.IO      as T
import qualified Data.Vector            as V
import           GHC.Generics

import           Brick.BChan
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.Yaml              as Y
import           Network.WebSockets     (ClientApp, Connection,
                                         ConnectionException (..),
                                         WebSocketsData (..), receiveData,
                                         sendClose, sendTextData)
import           Streaming              as S
import qualified Streaming.Prelude      as S
import           Text.Pretty.Simple
import           Wuss

import           Config
import           Http
import           Plugins
import           Plugins.Default
import           Rendering
import           Types
import           Types.Common
import           Types.Gateway

plugins :: [RunnablePlugin]
plugins =
    concat
    [ defaultPlugins
    -- , [ magic fooPlugin
    --   , magic barPlugin
    --   ]
    ]

updateSeqNo :: Maybe Int -> BotM ()
updateSeqNo Nothing = return ()
updateSeqNo (Just s) = do
    var <- gets seqNoVar
    liftIO $ atomically $
        isEmptyTMVar var >>= \case
            True -> putTMVar var s
            _ -> void $ swapTMVar var s
    return ()

wsSource :: MonadIO m => Connection -> Stream (Of B.ByteString) m ()
wsSource conn =
       S.repeatM (liftIO $ receiveData conn)

wsSink :: WebSocketsData a => Connection -> Stream (Of a) IO r -> IO r
wsSink conn = S.mapM_ (sendTextData conn)

parseCommand :: B.ByteString -> Either String RawGatewayCommand
parseCommand = eitherDecode


reportRawParseErrors
  :: Stream (Of String) (Stream (Of RawGatewayCommand) BotM) r
  -> Stream (Of RawGatewayCommand) BotM r
reportRawParseErrors streams = do
    logger <- gets logErr
    S.mapM_ (\err -> liftIO $ logger "Raw parse error" $ pack err) streams
  where

reportCommandParseErrors
    :: Stream (Of (RawGatewayCommand, String)) (Stream (Of GatewayCommand) BotM) r
    -> Stream (Of GatewayCommand) BotM r
reportCommandParseErrors streams = do
    logger <- gets logErr
    S.mapM_ (\(msg, err) -> liftIO $ logger (TL.toStrict $ pShowNoColor err) (TL.toStrict $ pShowNoColor msg)) streams
processGatewayCommands
  :: Stream (Of RawGatewayCommand) BotM r
  -> Stream (Of (Either (RawGatewayCommand, String) GatewayCommand)) BotM r
processGatewayCommands =
    S.map $ \x -> first (x,) $ rawToCommand x

queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)

startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO ()
startWriterThread gwq conn = do
    void $ forkIO $ wsSink conn $ queueSource gwq

app :: BotConfig -> Connection -> IO ()
app cfg conn = do
    -- putStrLn "Connected!"
    seqVar <- newEmptyTMVarIO
    sessionVar <- newEmptyTMVarIO
    htidvar <- newEmptyTMVarIO
    gatewayQueue <- newTQueueIO
    eventChan <- newBChan 1000
    startWriterThread gatewayQueue conn
    let botState =
            BotState
            { sessionIdVar = sessionVar
            , seqNoVar     = seqVar
            , botConfig    = cfg
            , gwQueue      = gatewayQueue
            , logInfo      = makeLogger eventChan MessageAdded
            , logErr       = makeLogger eventChan ErrorAdded
            , eventChan    = eventChan
            , heartbeatThreadId = htidvar
            }
    tid <- forkIO $ void $ flip runStateT botState $ runBotM $ do
            S.mapM_ (\raw -> mapM_ (runPlugins plugins) (d raw))
            $ S.chain (updateSeqNo . s) $ reportRawParseErrors
            $ S.partitionEithers
            $ S.map parseCommand
            $ wsSource conn
    renderInterface eventChan
    killThread tid
    return ()
  where
    makeLogger chan event title msg = do
        liftIO $ writeBChan chan $ event (title, msg)
        -- liftIO $ atomically (writeTQueue q (title, msg))

handleException :: ConnectionException -> IO ()
handleException e = do
    putStrLn "Oops!"
    pPrint e


main :: IO ()
main = do
    cfg <- readConfig "config.yaml"
    case cfg of
        Left ex -> do
            putStrLn $ Y.prettyPrintParseException ex
        Right cfg -> do
            gateway <- getGateway (botToken cfg)
            runSecureClient (drop 6 . unpack $ gwUrl gateway) 443 "/?v=6&&encoding=json" (app cfg) `catch` handleException
