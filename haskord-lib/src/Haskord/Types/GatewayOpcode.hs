{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Haskord.Types.GatewayOpcode where

import           Data.Aeson
import           Data.Scientific    (floatingOrInteger)
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
      deriving (Show, Eq)
  |])

instance FromJSON GatewayOpcode where
    parseJSON =
        withScientific "opcode" $ \n ->
            case floatingOrInteger n of
                Left _ ->
                    fail "Opcode was not an integer"
                Right i ->
                    maybe (fail "Unknown opcode") return $ lookup i opcodeMap

instance ToJSON GatewayOpcode where
    toJSON opcode =
        case lookup opcode reverseOpcodeMap of
            Just op -> toJSON op
            Nothing -> error $ "Couldn't find opcode " ++ show opcode
        -- toJSON $ fromJust (lookup opcode reverseOpcodeMap)


reverseOpcodeMap :: [(GatewayOpcode, Int)]
reverseOpcodeMap =
    map swap opcodeMap
  where
    swap (x, y) = (y, x)

opcodeMap :: [(Int, GatewayOpcode)]
opcodeMap =
    zip [0 .. 11] opcodes
  where
    opcodes =
        [ Dispatch
        , Heartbeat
        , Identify
        , StatusUpdate
        , VoiceStateUpdate
        , VoiceServerPing
        , Resume
        , Reconnect
        , RequestGuildMembers
        , InvalidSession
        , Hello
        , HeartbeatACK
        ]
