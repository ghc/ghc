{-# LANGUAGE StaticPointers, ScopedTypeVariables #-}
module T27664 where

import Data.Typeable
import GHC.StaticPtr

-- The free type variable 'a' must not trigger warning GHC-82529
f :: forall a. Typeable a => StaticPtr (a -> a)
f = static (id :: a -> a)
