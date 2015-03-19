{-# LANGUAGE OverloadedStrings #-}

module Kain where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Network.Mircy

type HostName = String
type Port = String

startKain :: HostName -> Port -> IO ()
startKain host port = runMircy host port prog

prog :: Mircy ()
prog = do
    getIRCMessage >>= handleMessage
    sendIRCCommand $ IRCNick "kain"
    sendIRCCommand $ IRCUser "kain" "8" "*" "kainbot"
    sendIRCCommand $ IRCJoin "bots"
    forever $ getIRCMessage >>= handleMessage

handleMessage :: IRCMessage -> Mircy ()
handleMessage = lift . print
