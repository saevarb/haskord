module Haskord.Http
    ( getGateway
    , DiscordReq (..)
    , runDiscordRequest
    ) where

import           Control.Exception     (throwIO)
import           Control.Monad.Reader
import           Data.ByteString       (ByteString)
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Typeable

import           Data.Aeson            (Value (..))
import           Data.Hashable
import           Network.HTTP.Req

import           Haskord.Types.Common
import           Haskord.Types.Gateway

instance MonadHttp IO where
    handleHttpException = throwIO

apiEndpoint :: ByteString
apiEndpoint = "https://discordapp.com/api/v6"

parsedUrl :: Url 'Https
Just (parsedUrl, _) = parseUrlHttps apiEndpoint

getGateway :: Text -> IO GatewayResponse
getGateway tok = do
    res <- req GET (parsedUrl /: "gateway" /: "bot") NoReqBody jsonResponse opts
    return $ responseBody res
  where
    opts =
        mconcat
        [ header "Authorization" ("Bot " <> encodeUtf8 tok)
        , header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"
        ]

sendRequest
  :: (MonadIO m, HttpMethod method,
      HttpBody body, HttpResponse response,
      HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
     Text
     -> method
     -> Url scheme
     -> body
     -> Proxy response
     -> Maybe (Option scheme)
     -> m (HttpResponseBody response)
sendRequest token method path body resp opts = do
    res <- liftIO $ req method
        path
        body
        resp
        (maybe defaultOptions (defaultOptions <>) opts)
    return $ responseBody res
 where
    defaultOptions =
            header "Authorization" ("Bot " <> encodeUtf8 token)
            <> header "User-Agent" "DiscordBot (https://github.com/saevarb/haskord, 0.1)"

type ChannelModify = ()
data DiscordReq a where
    GetChannel               :: Snowflake Channel -> DiscordReq Channel
    ModifyChannel            :: Snowflake Channel -> ChannelModify     -> DiscordReq Channel
    GetChannelMessagesAround :: Snowflake Channel -> Snowflake Message -> Int -> DiscordReq [Message]
    GetChannelMessagesBefore :: Snowflake Channel -> Snowflake Message -> Int -> DiscordReq [Message]
    GetChannelMessagesAfter  :: Snowflake Channel -> Snowflake Message -> Int -> DiscordReq [Message]
    GetChannelMessage        :: Snowflake Channel -> Snowflake Message -> DiscordReq Message
    CreateMessage            :: Snowflake Channel -> OutMessage        -> DiscordReq Message
    CreateReaction           :: Snowflake Channel -> Snowflake Message -> Text -> DiscordReq ()
    deriving (Typeable)

deriving instance Show (DiscordReq a)
deriving instance Eq (DiscordReq a)

hashReq :: Hashable a => Int -> Int -> a -> Int
hashReq s id_ a = hashWithSalt s (id_, a)

instance Hashable (DiscordReq a) where
    hashWithSalt s (GetChannel cid)                     = hashReq s 0 cid
    hashWithSalt s (ModifyChannel cid _)                = hashReq s 1 cid
    hashWithSalt s (GetChannelMessagesAround cid mid c) = hashReq s 2 (cid, mid, c)
    hashWithSalt s (GetChannelMessagesBefore cid mid c) = hashReq s 3 (cid, mid, c)
    hashWithSalt s (GetChannelMessagesAfter cid mid c)  = hashReq s 4 (cid, mid, c)
    hashWithSalt s (GetChannelMessage cid mid)          = hashReq s 5 (cid, mid)
    hashWithSalt s (CreateMessage cid _)                = hashReq s 6 cid
    hashWithSalt s (CreateReaction cid mid emo)         = hashReq s 7 (cid, mid, emo)


runDiscordRequest :: Text -> DiscordReq a -> IO a
runDiscordRequest t (CreateMessage cid msg) =
    sendRequest t POST (parsedUrl /: "channels" /~ cid /: "messages") (ReqBodyJson msg) jsonResponse Nothing
runDiscordRequest t (GetChannelMessage cid mid) =
    sendRequest t GET (parsedUrl /: "channels" /~ cid /: "messages" /~ mid) NoReqBody jsonResponse Nothing
runDiscordRequest t (GetChannelMessagesAround cid mid count) =
    sendRequest t GET (parsedUrl /: "channels" /~ cid /: "messages") NoReqBody jsonResponse $ Just $
        "around" =: mid
        <> "limit" =: count
runDiscordRequest t (CreateReaction cid mid emo) =
    sendRequest t PUT
        (parsedUrl /: "channels" /~ cid /: "messages" /~ mid /: "reactions" /~ emo /: "@me")
        NoReqBody ignoreResponse Nothing


-- getHaxlEnv :: GenHaxl u (Env u)
-- getHaxlEnv = GenHaxl $ \env -> done (Ok env)

