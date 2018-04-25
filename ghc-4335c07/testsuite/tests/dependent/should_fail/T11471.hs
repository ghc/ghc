{-# LANGUAGE MagicHash, PolyKinds, TypeFamilies, AllowAmbiguousTypes #-}

module T11471 where

import GHC.Exts
import Data.Proxy

type family F a :: k

type instance F Int = Int#

f :: Proxy a -> F a -> F a
f _ x = x

bad = f (undefined :: Proxy Int#) 3#
