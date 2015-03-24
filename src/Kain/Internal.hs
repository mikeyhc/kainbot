{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Kain.Internal where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Mircy
import           System.IO

type HostName = String
type Port = String

data KainState = KainState
    { kainAuthUser :: Maybe B.ByteString
    , kainUserList :: M.Map B.ByteString B.ByteString
    }


data KainHandlerState = KainHandlerState
    { kainHandlerNick :: B.ByteString
    , kainHandlerUser :: B.ByteString
    , kainHandlerChan :: B.ByteString
    , kainHandlerMsg  :: B.ByteString
    , kainHandlerKain :: KainState
    }

newtype KainT m a = KainT (StateT KainState (MircyT m) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type Kain a = KainT IO a

instance MonadTrans KainT where
    lift = KainT . lift . lift

instance (Monad m) => MonadState KainState (KainT m) where
    state = KainT . state

instance (Monad m) => MonadMircy (KainT m) where
    getIRCHandle = KainT $ lift getIRCHandle

newtype KainHandlerT m a = KainHandlerT (StateT KainHandlerState (KainT m) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type KainHandler a = KainHandlerT IO a

instance MonadTrans KainHandlerT where
    lift = KainHandlerT . lift . lift

instance (Monad m) => MonadState KainHandlerState (KainHandlerT m) where
    state = KainHandlerT . state

instance (Monad m) => MonadMircy (KainHandlerT m) where
    getIRCHandle = KainHandlerT . lift . KainT $ lift getIRCHandle

runKainT :: (Monad m) => Handle -> KainT m a -> m a
runKainT h (KainT s) = runMircyT (evalStateT s (KainState Nothing M.empty)) h

runKain :: HostName -> Port -> Kain () -> IO ()
runKain h p (KainT s) = runMircy h p (evalStateT s (KainState Nothing M.empty))

runKainHandlerT :: (Monad m) => B.ByteString -> B.ByteString -> B.ByteString
                -> B.ByteString -> KainHandlerT m a -> KainT m a
runKainHandlerT nick user chan msg (KainHandlerT s) = do
    state <- get
    (ret, s) <- runStateT s (KainHandlerState nick user chan msg state)
    put state
    return ret

runKainHandler :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
               -> KainHandler a -> Kain a
runKainHandler = runKainHandlerT

setNick :: B.ByteString -> B.ByteString -> Kain ()
setNick user nick = modify (\k -> k
    { kainUserList = M.insert user nick (kainUserList k) })

getNick :: B.ByteString -> Kain (Maybe B.ByteString)
getNick u = gets $ M.lookup u . kainUserList
