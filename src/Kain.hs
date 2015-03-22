{-# LANGUAGE OverloadedStrings #-}

module Kain where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Kain.Internal
import           Network.Mircy
import           System.Exit

startKain :: HostName -> Port -> IO ()
startKain host port = runKain host port prog

registerNick :: Kain ()
registerNick = sendIRCCommand $ IRCNick "kain"

registerUser :: Kain ()
registerUser = sendIRCCommand $ IRCUser "kain" "8" "*" "kainbot"

joinBots :: Kain ()
joinBots = sendIRCCommand $ IRCJoin "#bots"

prog :: Kain ()
prog = do
    getIRCMessage >>= handleMessage
    registerNick
    registerUser
    forever $ getIRCMessage >>= handleMessage

handleMessage :: IRCMessage -> Kain ()
handleMessage (IRCNotice typ msg)         = showNotice typ msg
handleMessage (IRCReply code cmd msg)     = showReply code cmd msg
                                         >> handleReply code cmd msg
handleMessage (IRCError code cmd msg)     = showError code cmd msg
                                         >> handleError code cmd msg
handleMessage (IRCMsg nick user chan msg) = handlePrivMsg nick user chan msg
handleMessage (IRCJoinMsg chan)           = handleJoinMsg chan
handleMessage (IRCNickMsg user _ nick)    = handleNickMsg user nick
handleMessage m                           = lift $ print m

showReply :: Int -> B.ByteString -> B.ByteString -> Kain ()
showReply code cmd msg = lift . B.putStrLn $ foldl1 B.append wordlist
  where
    wordlist = [ "reply(", B.pack (show code), ", ", cmd, "): ", msg]

showNotice :: B.ByteString -> B.ByteString -> Kain ()
showNotice t m = lift . B.putStrLn $ foldl1 B.append [ "notice(", t, "): ", m ]

showError :: Int -> B.ByteString -> B.ByteString -> Kain ()
showError code cmd msg = lift . B.putStrLn $ foldl1 B.append wordlist
  where
    wordlist = [ "error(", B.pack (show code), ", ", cmd, "): ", msg]

handleError :: Int -> B.ByteString -> B.ByteString -> Kain ()
handleError 451 _ _ = registerUser >> joinBots
handleError _   _ _ = return ()

handleReply :: Int -> B.ByteString -> B.ByteString -> Kain ()
handleReply 376 _ _ = joinBots
handleReply 352 _ m = handleWhoReply m
handleReply _   _ _ = return ()

handlePrivMsg :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
              -> Kain ()
handlePrivMsg nick user chan msg =
    when ("kain:" `B.isPrefixOf` msg) $ do
        let message = B.dropWhile (== ' ') $ B.drop 5 msg
        lift . B.putStrLn $ foldl1 B.append
            [ "privmsg ", nick, "(", user, "): ", message ]
        handlePrivMsg' nick user chan message

handleJoinMsg :: B.ByteString -> Kain ()
handleJoinMsg _ = sendIRCCommand $ IRCWho Nothing

handleNickMsg :: B.ByteString -> B.ByteString -> Kain ()
handleNickMsg = setNick

dropWord :: B.ByteString -> B.ByteString
dropWord = B.dropWhile (== ' ') . B.dropWhile (/= ' ')

takeWord :: B.ByteString -> B.ByteString
takeWord = B.takeWhile (/= ' ')

handleWhoReply :: B.ByteString -> Kain ()
handleWhoReply m = let temp1 = dropWord m
                       user1 = takeWord temp1
                       temp2 = dropWord temp1
                       user  = B.append (B.snoc user1 '@') (takeWord temp2)
                       nick  = takeWord . dropWord $ dropWord temp2
                   in setNick user nick

handlePrivMsg' :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
               -> Kain ()
handlePrivMsg' nick user chan msg 
    | "god" == lmsg                 = showGod nick chan
    | "auth " `B.isPrefixOf` lmsg   = doAuth nick user chan (dropWord msg)
    | "unauth" == lmsg              = ifauth nick user chan
                                    $ doUnauth nick chan
    | "quit" == lmsg                = ifauth nick user chan $ do
                                        sendIRCCommand (IRCQuit quitmsg)
                                        lift exitSuccess
    | "localnick" == lmsg           = gets kainUserList >>= lift . print
    | otherwise                     = return ()
  where
    lmsg = B.map toLower msg
    quitmsg = Just "I'll be back"

ifauth :: B.ByteString -> B.ByteString -> B.ByteString -> Kain () -> Kain ()
ifauth nick user chan f = do
    mauthuser <- gets kainAuthUser
    case mauthuser of
        Nothing -> authfailed
        Just u  -> if u == user then f
                                else authfailed
  where
    authfailed = sendIRCCommand . IRCPrivMsg chan
               $ B.append nick ": you are not authenticated for that"

showGod :: B.ByteString -> B.ByteString -> Kain ()
showGod nick chan = do
    mauthuser <- gets kainAuthUser
    case mauthuser of
        Just u  -> sendIRCCommand . IRCPrivMsg chan
                                  $ foldl1 B.append [nick, ": ", u, " is god" ]
        Nothing -> sendIRCCommand . IRCPrivMsg chan
                                  $ B.append nick ": there is no god"

doAuth :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
       -> Kain ()
doAuth nick user chan pass = do
    state <- get
    case kainAuthUser state of
        Just _  -> sendIRCCommand . IRCPrivMsg chan
                                  $ B.append nick
                                  ": someone has already authenticated"
        Nothing -> if pass == "kainpass"
            then do
                put $ state { kainAuthUser = Just user }
                sendIRCCommand . IRCPrivMsg chan
                               $ B.append nick ": successfully authenticated"
            else sendIRCCommand . IRCPrivMsg chan
                                $ B.append nick ": incorrect password"

doUnauth :: B.ByteString -> B.ByteString -> Kain ()
doUnauth nick chan = do
    modify (\s -> s { kainAuthUser = Nothing })
    sendIRCCommand . IRCPrivMsg chan
                   $ B.append nick ": successfully unauthenticated"
