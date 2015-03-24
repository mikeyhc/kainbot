{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Kain.Internal where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Network.Mircy
import           System.IO

type HostName = String
type Port = String

data KainState = KainState
    { _kainAuthUser :: Maybe B.ByteString
    , _kainUserList :: M.Map B.ByteString B.ByteString
    }


data KainHandlerState = KainHandlerState
    { kainHandlerNick :: B.ByteString
    , kainHandlerUser :: B.ByteString
    , kainHandlerChan :: B.ByteString
    , kainHandlerMsg  :: B.ByteString
    , kainHandlerKain :: KainState
    }

data KainPluginState a  = KainPluginState
    { kainPluginData    :: a
    , kainPluginHandler :: KainHandlerState
    , kainPluginKain    :: KainState
    }

class KainStateMonad m where
    getKainState    :: m KainState
    putKainState    :: KainState -> m ()
    modifyKainState :: (KainState -> KainState) -> m ()

class KainHandlerMonad m where
    getKainHandler    :: m KainHandlerState
    putKainHandler    :: KainHandlerState -> m ()
    modifyKainHandler :: (KainHandlerState -> KainHandlerState) -> m ()

class KainPluginMonad m where
    getKainPlugin    :: m (KainPluginState a)
    putKainPlugin    :: KainPluginState a -> m ()
    modifyKainPlugin :: (KainPluginState a -> KainPluginState a) -> m ()

newtype KainT m a = KainT (StateT KainState (MircyT m) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type Kain a = KainT IO a

instance MonadTrans KainT where
    lift = KainT . lift . lift

instance (Monad m) => MonadState KainState (KainT m) where
    state = KainT . state

instance (Monad m) => MonadMircy (KainT m) where
    getIRCHandle = KainT $ lift getIRCHandle

instance (Monad m) => KainStateMonad (KainT m) where
    getKainState    = get
    putKainState    = put
    modifyKainState = modify

newtype KainHandlerT m a = KainHandlerT (StateT KainHandlerState (KainT m) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type KainHandler a = KainHandlerT IO a

instance MonadTrans KainHandlerT where
    lift = KainHandlerT . lift . lift

instance (Monad m) => MonadState KainHandlerState (KainHandlerT m) where
    state = KainHandlerT . state

instance (Monad m) => MonadMircy (KainHandlerT m) where
    getIRCHandle = KainHandlerT . lift . KainT $ lift getIRCHandle

instance (Monad m) => KainStateMonad (KainHandlerT m) where
    getKainState = gets kainHandlerKain
    putKainState k = modify (\s -> s { kainHandlerKain = k })
    modifyKainState f = modify (\s -> s { kainHandlerKain
                                      = f (kainHandlerKain s) })

runKainT :: (Monad m) => Handle -> KainT m a -> m a
runKainT h (KainT s) = runMircyT (evalStateT s (KainState Nothing M.empty)) h

runKain :: HostName -> Port -> Kain () -> IO ()
runKain h p (KainT s) = runMircy h p (evalStateT s (KainState Nothing M.empty))

runKainHandlerT :: (Monad m) => B.ByteString -> B.ByteString -> B.ByteString
                -> B.ByteString -> KainHandlerT m a -> KainT m a
runKainHandlerT nick user chan msg (KainHandlerT s) = do
    state <- get
    (ret, s) <- runStateT s (KainHandlerState nick user chan msg state)
    put $ kainHandlerKain s
    return ret

runKainHandler :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
               -> KainHandler a -> Kain a
runKainHandler = runKainHandlerT

kainAuthUser :: (KainStateMonad m, Monad m) => m (Maybe B.ByteString)
kainAuthUser = liftM _kainAuthUser getKainState

setKainAuthUser :: (KainStateMonad m, Monad m) => Maybe B.ByteString -> m ()
setKainAuthUser u = modifyKainState (\s -> s { _kainAuthUser = u })

kainUserList :: (KainStateMonad m, Monad m)
             => m (M.Map B.ByteString B.ByteString)
kainUserList = liftM _kainUserList getKainState

setNick :: (KainStateMonad m, Monad m) => B.ByteString -> B.ByteString -> m ()
setNick user nick = modifyKainState (\k -> k
    { _kainUserList = M.insert user nick (_kainUserList k) })

getNick :: (KainStateMonad m, Monad m)
        => B.ByteString -> m (Maybe B.ByteString)
getNick u = liftM (M.lookup u) kainUserList
