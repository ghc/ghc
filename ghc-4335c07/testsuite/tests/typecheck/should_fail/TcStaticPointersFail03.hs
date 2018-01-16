{-# LANGUAGE StaticPointers     #-}

module StaticPointersFail03 where

import GHC.StaticPtr
import Data.Typeable

f1 :: (Typeable a, Typeable m, Monad m) => a -> m a
f1 = deRefStaticPtr (static return)
