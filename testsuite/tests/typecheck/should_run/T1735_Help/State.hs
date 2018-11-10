
module T1735_Help.State where

import Control.Monad (ap, liftM)


newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Monad m => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'

instance MonadFail m => MonadFail (StateT s m) where
    fail s = StateT $ \_ -> fail s

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

