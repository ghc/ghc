{-# LANGUAGE StaticPointers #-}
module T24464 where

import GHC.StaticPtr

class C a where
  f :: a -> StaticPtr ()
  f _ = static ()
