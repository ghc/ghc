-- Transactional memory for sequential implementations.
-- Transactions do not run concurrently, but are atomic in the face
-- of exceptions.

{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-- #hide
module Control.Sequential.STM (
        STM, atomically, throwSTM, catchSTM,
        TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar
    ) where

#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative (Applicative(pure, (<*>)))
#endif
import Control.Exception
import Data.IORef

-- The reference contains a rollback action to be executed on exceptions
newtype STM a = STM (IORef (IO ()) -> IO a)

unSTM :: STM a -> IORef (IO ()) -> IO a
unSTM (STM f) = f

instance Functor STM where
    fmap f (STM m) = STM (fmap f . m)

instance Applicative STM where
    pure = STM . const . pure
    STM mf <*> STM mx = STM $ \ r -> mf r <*> mx r

instance Monad STM where
    return = pure
    STM m >>= k = STM $ \ r -> do
        x <- m r
        unSTM (k x) r

atomically :: STM a -> IO a
atomically (STM m) = do
    r <- newIORef (return ())
    m r `onException` do
        rollback <- readIORef r
        rollback

throwSTM :: Exception e => e -> STM a
throwSTM = STM . const . throwIO

catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM (STM m) h = STM $ \ r -> do
    old_rollback <- readIORef r
    writeIORef r (return ())
    res <- try (m r)
    rollback_m <- readIORef r
    case res of
        Left ex -> do
            rollback_m
            writeIORef r old_rollback
            unSTM (h ex) r
        Right a -> do
            writeIORef r (rollback_m >> old_rollback)
            return a

newtype TVar a = TVar (IORef a)
    deriving (Eq)

newTVar :: a -> STM (TVar a)
newTVar a = STM (const (newTVarIO a))

newTVarIO :: a -> IO (TVar a)
newTVarIO a = do
    ref <- newIORef a
    return (TVar ref)

readTVar :: TVar a -> STM a
readTVar (TVar ref) = STM (const (readIORef ref))

readTVarIO :: TVar a -> IO a
readTVarIO (TVar ref) = readIORef ref

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ref) a = STM $ \ r -> do
    oldval <- readIORef ref
    modifyIORef r (writeIORef ref oldval >>)
    writeIORef ref a
