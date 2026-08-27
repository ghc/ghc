{-# LANGUAGE StaticPointers, ScopedTypeVariables #-}
module Repro where

import Data.Typeable
import GHC.StaticPtr

f1 :: forall a. Typeable a => StaticPtr (a -> a)
f1 = static (id :: a -> a)

f2 :: forall a. Typeable a => StaticPtr (a -> a)
f2 = static (id) :: StaticPtr (a->a)
