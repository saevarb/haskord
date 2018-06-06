module Haskord.WebSocket
    ( wsSource
    , updateSeqNo
    , startWriterThread
    , reportRawParseErrors
    ) where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as B

import           Network.WebSockets   (Connection, WebSocketsData (..),
                                       receiveData, sendTextData)
import           Streaming            as S
import qualified Streaming.Prelude    as S

import           Haskord.Prelude
import           Haskord.Types

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

reportRawParseErrors
  :: Stream (Of String) (Stream (Of SomeMessage) BotM) r
  -> Stream (Of SomeMessage) BotM r
reportRawParseErrors =
    S.mapM_ (logW' "Raw parse error")

queueSource :: TQueue a -> Stream (Of a) IO r
queueSource q = S.repeatM (liftIO . atomically $ readTQueue q)


startWriterThread :: WebSocketsData a => TQueue a -> Connection -> IO (Async ())
startWriterThread gwq conn =
    async $ wsSink conn $ queueSource gwq
