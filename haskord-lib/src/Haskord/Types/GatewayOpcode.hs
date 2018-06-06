{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Haskord.Types.GatewayOpcode where

import           Data.Singletons.TH

$(singletons [d|
  data GatewayOpcode
      = Dispatch
      | Heartbeat
      | Identify
      | StatusUpdate
      | VoiceStateUpdate
      | VoiceServerPing
      | Resume
      | Reconnect
      | RequestGuildMembers
      | InvalidSession
      | Hello
      | HeartbeatACK
      | UnknownError
      | UnknownOpcode
      deriving (Show, Eq)
  |])
