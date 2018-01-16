{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE ImpredicativeTypes #-}

module StaticPointersFail02 where

import GHC.StaticPtr

f1 :: StaticPtr ((forall a . a -> a) -> b)
f1 = static (undefined :: (forall a . a -> a) -> b)

f2 :: StaticPtr (Monad m => a -> m a)
f2 = static return
