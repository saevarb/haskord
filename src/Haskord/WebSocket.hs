{-# LANGUAGE LambdaCase #-}
module Haskord.WebSocket where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as B
import qualified Data.Text.Lazy           as TL (toStrict, unlines, unpack)

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Network.WebSockets       (ClientApp, Connection,
                                           ConnectionException (..),
                                           WebSocketsData (..), receiveData,
                                           sendClose, sendTextData)
import           Streaming                as S
import qualified Streaming.Prelude        as S
import           Text.Pretty.Simple
import Data.Aeson (eitherDecode)

import           Haskord.Plugins

updateSeqNo :: Maybe Int -> BotM ()
updateSeqNo Nothing = return ()
updateSeqNo (Just s) = do
    var <- asks seqNoVar
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
    S.mapM_ (logW' "Raw parse error") streams
  where

reportCommandParseErrors
    :: Stream (Of (RawGatewayCommand, String)) (Stream (Of GatewayCommand) BotM) r
    -> Stream (Of GatewayCommand) BotM r
reportCommandParseErrors streams = do
    S.mapM_ (\(msg, err) -> logE' (TL.toStrict $ pShowNoColor err) msg) streams

queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)


startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO (Async ())
startWriterThread gwq conn =
    async $ wsSink conn $ queueSource gwq
