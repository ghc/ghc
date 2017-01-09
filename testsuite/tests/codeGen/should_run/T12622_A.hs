-- A.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StaticPointers #-}
module T12622_A where

import Data.Typeable
import GHC.StaticPtr

g :: a -> Bool
g _ = True

data T a = T {-# UNPACK #-} !(StaticPtr a)

sg :: Typeable a => T (a -> Bool)
sg = T (static g)
