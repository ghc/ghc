{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module T7729a where

class Monad m => PrimMonad m where
  type PrimState m

class MonadTrans t where
  lift :: Monad m => m a -> t m a

class (PrimMonad (BasePrimMonad m), Monad m) => MonadPrim m where
  type BasePrimMonad m :: * -> *
  liftPrim :: BasePrimMonad m a -> m a


newtype Rand m a = Rand {
  runRand :: Maybe (m ()) -> m a
  }

instance (Monad m) => Monad (Rand m) where
  return           = Rand . const . return
  (Rand rnd) >>= f = Rand $ \g -> (\x -> runRand (f x) g) =<< rnd g

instance MonadTrans Rand where
  lift = Rand . const

instance MonadPrim m => MonadPrim (Rand m) where
  type BasePrimMonad (Rand m) = BasePrimMonad m
  liftPrim x = liftPrim (lift x)   -- This line changed from T7729