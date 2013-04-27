-- Transactional memory for sequential implementations.
-- Transactions do not run concurrently, but are atomic in the face
-- of exceptions.

{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

-- #hide
module LwConc.Sequential.PTM (
	PTM, atomically, throwPTM, catchPTM,
	TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar
    ) where

#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Exception
import Data.IORef

-- The reference contains a rollback action to be executed on exceptions
newtype PTM a = PTM (IORef (IO ()) -> IO a)

unPTM :: PTM a -> IORef (IO ()) -> IO a
unPTM (PTM f) = f

instance Functor PTM where
    fmap f (PTM m) = PTM (fmap f . m)

instance Applicative PTM where
    pure = PTM . const . pure
    PTM mf <*> PTM mx = PTM $ \ r -> mf r <*> mx r

instance Monad PTM where
    return = pure
    PTM m >>= k = PTM $ \ r -> do
	x <- m r
	unPTM (k x) r

#ifdef BASE4
atomically :: PTM a -> IO a
atomically (PTM m) = do
    r <- newIORef (return ())
    m r `onException` do
	rollback <- readIORef r
	rollback
#else
atomically :: PTM a -> IO a
atomically (PTM m) = do
    r <- newIORef (return ())
    m r `catch` \ (ex::SomeException) -> do
	rollback <- readIORef r
	rollback
	throw ex
#endif

throwPTM :: Exception e => e -> PTM a
throwPTM = PTM . const . throwIO

catchPTM :: Exception e => PTM a -> (e -> PTM a) -> PTM a
catchPTM (PTM m) h = PTM $ \ r -> do
    old_rollback <- readIORef r
    writeIORef r (return ())
    res <- try (m r)
    rollback_m <- readIORef r
    case res of
	Left ex -> do
	    rollback_m
	    writeIORef r old_rollback
	    unPTM (h ex) r
	Right a -> do
	    writeIORef r (rollback_m >> old_rollback)
	    return a

newtype TVar a = TVar (IORef a)
    deriving (Eq)

newTVar :: a -> PTM (TVar a)
newTVar a = PTM (const (newTVarIO a))

newTVarIO :: a -> IO (TVar a)
newTVarIO a = do
    ref <- newIORef a
    return (TVar ref)

readTVar :: TVar a -> PTM a
readTVar (TVar ref) = PTM (const (readIORef ref))

readTVarIO :: TVar a -> IO a
readTVarIO (TVar ref) = readIORef ref

writeTVar :: TVar a -> a -> PTM ()
writeTVar (TVar ref) a = PTM $ \ r -> do
    oldval <- readIORef ref
    modifyIORef r (writeIORef ref oldval >>)
    writeIORef ref a
