{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             FlexibleInstances #-}

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
    { _kainHandlerNick :: B.ByteString
    , _kainHandlerUser :: B.ByteString
    , _kainHandlerChan :: B.ByteString
    , _kainHandlerMsg  :: B.ByteString
    , _kainHandlerKain :: KainState
    }

data KainPluginState a  = KainPluginState
    { _kainPluginData    :: a
    , _kainPluginHandler :: KainHandlerState
    }

class KainStateMonad m where
    getKainState    :: m KainState
    putKainState    :: KainState -> m ()
    modifyKainState :: (KainState -> KainState) -> m ()

class KainHandlerMonad m where
    getKainHandler    :: m KainHandlerState
    putKainHandler    :: KainHandlerState -> m ()
    modifyKainHandler :: (KainHandlerState -> KainHandlerState) -> m ()

class KainPluginMonad b m where
    getKainPlugin    :: m (KainPluginState b)
    putKainPlugin    :: KainPluginState b -> m ()
    modifyKainPlugin :: (KainPluginState b -> KainPluginState b) -> m ()

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
    getIRCHandle = KainHandlerT . lift $ getIRCHandle

instance (Monad m) => KainStateMonad (KainHandlerT m) where
    getKainState = gets _kainHandlerKain
    putKainState k = modify (\s -> s { _kainHandlerKain = k })
    modifyKainState f = modify (\s -> s { _kainHandlerKain
                                      = f (_kainHandlerKain s) })

instance (Monad m) => KainHandlerMonad (KainHandlerT m) where
    getKainHandler    = get
    putKainHandler    = put
    modifyKainHandler = modify

newtype KainPluginT b m a = KainPluginT (StateT (KainPluginState b)
                                                (KainHandlerT m) a)
    deriving (Functor, Applicative, Monad, MonadIO)

type KainPlugin b a = KainPluginT (StateT (KainPluginState b)
                                  (KainHandlerT IO) a)

instance MonadTrans (KainPluginT a) where
    lift = KainPluginT . lift . lift

instance (Monad m) => MonadState (KainPluginState b) (KainPluginT b m) where
    state = KainPluginT . state

instance (Monad m) => MonadMircy (KainPluginT b m) where
    getIRCHandle = KainPluginT . lift $ getIRCHandle

instance (Monad m) => KainStateMonad (KainPluginT b m) where
    getKainState      = liftM _kainHandlerKain getKainHandler
    putKainState k    = do
        khs <- getKainHandler
        putKainHandler $ khs { _kainHandlerKain = k }
    modifyKainState f = do
        khs <- getKainState
        putKainState (f khs)

instance (Monad m) => KainHandlerMonad (KainPluginT b m) where
    getKainHandler      = gets _kainPluginHandler
    putKainHandler h    = modify (\s -> s { _kainPluginHandler = h })
    modifyKainHandler f = modify (\s -> s { _kainPluginHandler
                                          = f (_kainPluginHandler s) })

instance (Monad m) => KainPluginMonad b (KainPluginT b m) where
    getKainPlugin    = get
    putKainPlugin    = put
    modifyKainPlugin = modify

runKainT :: (Monad m) => Handle -> KainT m a -> m a
runKainT h (KainT s) = runMircyT (evalStateT s (KainState Nothing M.empty)) h

runKain :: HostName -> Port -> Kain () -> IO ()
runKain h p (KainT s) = runMircy h p (evalStateT s (KainState Nothing M.empty))

runKainHandlerT :: (Monad m) => B.ByteString -> B.ByteString -> B.ByteString
                -> B.ByteString -> KainHandlerT m a -> KainT m a
runKainHandlerT nick user chan msg (KainHandlerT s) = do
    state <- get
    (ret, rs) <- runStateT s (KainHandlerState nick user chan msg state)
    put $ _kainHandlerKain rs
    return ret

runKainHandler :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
               -> KainHandler a -> Kain a
runKainHandler = runKainHandlerT

runKainPluginT :: (Monad m) => b -> KainPluginT b m a -> KainHandlerT m a
runKainPluginT p (KainPluginT s) = do
    state <- getKainHandler
    (ret, rs) <- runStateT s (KainPluginState p state)
    putKainHandler $ _kainPluginHandler rs
    return ret

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
