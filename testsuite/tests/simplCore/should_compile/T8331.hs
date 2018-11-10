{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Main ( main, useAbstractMonad ) where

import Control.Monad
import Control.Monad.ST
import Control.Applicative

newtype ReaderT r m a = ReaderT {
        -- | The underlying computation, as a function of the environment.
        runReaderT :: r -> m a
    }

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)

instance (Monad m) => Monad (ReaderT r m) where
    return x  = ReaderT (\_ -> return x)
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r

instance MonadFail m => MonadFail (ReaderT r m) where
    fail msg = ReaderT (\_ -> fail msg)

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

class (Applicative m, Functor m , Monad m) => MonadAbstractIOST m where
    addstuff :: Int -> m Int

type ReaderST s = ReaderT (Int) (ST s)

instance MonadAbstractIOST (ReaderST s) where
    addstuff a = return . (a +) =<< ask

runAbstractST :: (forall s. ReaderST s a) -> a
runAbstractST f = runST $ runReaderT f 99

{-# SPECIALIZE useAbstractMonad :: Int -> ReaderST s Int #-}
-- Note the polymorphism
useAbstractMonad :: MonadAbstractIOST m => Int -> m Int
useAbstractMonad n = foldM (\a b -> a `seq` return . (a +) =<< (addstuff b)) 0 [1..n]

-- useConcreteMonad :: Int -> ReaderST s Int
-- useConcreteMonad = foldM (\a b -> a `seq` return . (a +) =<< (addstuff b)) 0 [1..n]

main :: IO ()
main = do
    let st = runAbstractST (useAbstractMonad 5000000)
    putStrLn . show $ st
