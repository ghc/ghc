{-# LANGUAGE StaticPointers #-}

module RnStaticPointersFail03 where

import Data.Typeable
import GHC.StaticPtr

f x = static (x . id)

f0 x = static (k . id)
  where
    k = const (const () x)

f1 x = static (k . id)
  where
    k = id

f2 :: Typeable a => a -> StaticPtr TypeRep
f2 x = const (static (g undefined)) (h x)
  where
    g = h
    h = typeOf
