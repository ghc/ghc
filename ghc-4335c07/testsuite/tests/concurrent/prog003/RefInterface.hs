{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module RefInterface where


import Control.Monad



class Monad m => Ref r m | m -> r, r -> m where
   newRef :: a -> m (r a)
   readRef :: (r a) -> m a
   writeRef :: (r a) -> a -> m ()
   atomicRef :: m a -> IO a
   retryRef :: m a
