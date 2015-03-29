{-# LANGUAGE OverloadedStrings #-}

module Kain where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Kain.Internal
import           Network.Mircy
import           System.Exit

startKain :: String -> String -> HostName -> Port -> IO ()
startKain nick pass host port =
    runKain (B.pack nick) (B.pack pass) host port prog

registerNick :: Kain ()
registerNick = kainNick >>= sendIRCCommand . IRCNick

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
handleMessage (IRCNotice typ msg)           = showNotice typ msg
handleMessage (IRCReply code cmd msg)       = showReply code cmd msg
                                           >> handleReply code cmd msg
handleMessage (IRCError code cmd msg)       = showError code cmd msg
                                           >> handleError code cmd msg
handleMessage (IRCMsg nick user chan msg)   = showPrivMsg nick user chan msg
                                   >> handlePrivMsg nick user chan msg
handleMessage m@(IRCJoinMsg _ "kain" chan)  = lift (print m)
                                           >> handleJoinMsg chan
handleMessage m@(IRCJoinMsg user nick _)    = lift (print m)
                                           >> handleNewUser user nick
handleMessage (IRCNickMsg user _ nick)      = handleNickMsg user nick
handleMessage m                             = lift $ print m

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

showPrivMsg :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
            -> Kain ()
showPrivMsg nick user chan msg = lift . B.putStrLn $ foldl1 B.append wordlist
  where
    wordlist = [ "privmsg(", nick, "(", user, "), ", chan, "): ", msg ]

handleError :: Int -> B.ByteString -> B.ByteString -> Kain ()
handleError 451 _ _ = registerUser >> joinBots
handleError _   _ _ = return ()

handleReply :: Int -> B.ByteString -> B.ByteString -> Kain ()
handleReply 376 _ _ = joinBots
handleReply 352 _ m = handleWhoReply m
handleReply _   _ _ = return ()

handlePrivMsg :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
              -> Kain ()
handlePrivMsg nick user chan msg = do
    currentNick <- kainNick
    let nickWithCol = B.append currentNick ":"
    when (chan == currentNick || nickWithCol `B.isPrefixOf` msg) $ do
        let message = if nickWithCol `B.isPrefixOf` msg then dropWord msg
                                                        else msg
        lift . B.putStrLn $ foldl1 B.append
            [ "privmsg ", nick, "(", user, "): ", message ]
        runKainHandler nick user chan message $ handlePrivMsg' message

handleJoinMsg :: B.ByteString -> Kain ()
handleJoinMsg _ = sendIRCCommand $ IRCWho Nothing

handleNickMsg :: B.ByteString -> B.ByteString -> Kain ()
handleNickMsg = setNick

handleNewUser :: B.ByteString -> B.ByteString -> Kain ()
handleNewUser = setNick

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

sendReply :: B.ByteString -> KainHandler ()
sendReply msg = do
    chan <- getHandlerChan
    nick <- getHandlerNick
    currentNick <- kainNick
    if chan == currentNick
        then sendIRCCommand $ IRCPrivMsg nick msg
        else sendIRCCommand . IRCPrivMsg chan . B.append nick
                                              $ B.append ": " msg

handlePrivMsg' :: B.ByteString -> KainHandler ()
handlePrivMsg' msg
    | "god" == lmsg                 = showGod
    | "auth " `B.isPrefixOf` lmsg   = doAuth
    | "unauth" == lmsg              = ifauth doUnauth
    | "quit" == lmsg                = ifauth $ do
                                        sendIRCCommand (IRCQuit quitmsg)
                                        lift exitSuccess
    | "localnick" == lmsg           = kainUserList >>= lift . print
    | otherwise                     = return ()
  where
    lmsg = B.map toLower msg
    quitmsg = Just "I'll be back"

ifauth :: KainHandler () -> KainHandler ()
ifauth f = do
    mauthuser <- kainAuthUser
    user <- getHandlerUser
    case mauthuser of
        Nothing -> authfailed
        Just u  -> if u == user then f
                                else authfailed
  where
    authfailed = sendReply "you are not authenticated for that"

showGod :: KainHandler ()
showGod = do
    mauthuser <- kainAuthUser
    case mauthuser of
        Just u  -> do
            manick <- getNick u
            case manick of
                Just n  -> sendReply $ god n
                Nothing -> sendReply $ god u
        Nothing -> sendReply nogod
  where
    nogod = "there is no god"
    god = (`B.append` " is god")

doAuth :: KainHandler ()
doAuth = do
    mauser <- kainAuthUser
    user <- getHandlerUser
    pass <- kainPassword
    pass' <- dropWord <$> getHandlerMsg
    case mauser of
        Just _  -> sendReply "someone has already authenticated"
        Nothing -> if pass == pass'
            then do
                setKainAuthUser $ Just user
                sendReply "successfully authenticated"
            else sendReply "incorrect password"

doUnauth :: KainHandler ()
doUnauth = do
    setKainAuthUser Nothing
    sendReply "successfully unauthenticated"
