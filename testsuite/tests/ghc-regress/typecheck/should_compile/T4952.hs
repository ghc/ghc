{-# LANGUAGE UndecidableInstances,
             MultiParamTypeClasses,
             KindSignatures,
             FlexibleInstances,
             FunctionalDependencies #-}

module Storage.Hashed.Monad () where

class Monad m => TreeRO m where
    withDirectory :: (MonadError e m) => Int -> m a -> m a
    expandTo :: (MonadError e m) => Int -> m Int

instance (Monad m, MonadError e m) => TreeRO (M m) where
    expandTo = undefined
    withDirectory dir _ = do
      _ <- expandTo dir
      undefined

data M (m :: * -> *) a

instance Monad m => Monad (M m) where
    (>>=) = undefined
    return = undefined

instance MonadError e m => MonadError e (M m)

class Monad m => MonadError e m | m -> e
