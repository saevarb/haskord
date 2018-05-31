module Haskord.WebSocket where

import           Control.Monad.State
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text.Lazy         as TL (toStrict, unlines, unpack)

import           Streaming              as S
import qualified Streaming.Prelude      as S
import           Network.WebSockets     (ClientApp, Connection,
                                         ConnectionException (..),
                                         WebSocketsData (..), receiveData,
                                         sendClose, sendTextData)
import           Text.Pretty.Simple
import           Control.Concurrent.STM
import Control.Concurrent.Async

import           Haskord.Types
import           Haskord.Types.Common
import           Haskord.Types.Gateway
import Haskord.Plugins

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
  :: Stream (Of String) (Stream (Of SomeMessage) BotM) r
  -> Stream (Of SomeMessage) BotM r
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
queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)


startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO ()
startWriterThread gwq conn =
    void $ async $ wsSink conn $ queueSource gwq
