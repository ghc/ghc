{-# LANGUAGE MagicHash, PolyKinds, TypeFamilies, AllowAmbiguousTypes, DataKinds #-}

module T11471 where

import GHC.Exts
import Data.Proxy
import Data.Kind

type family F a :: k

type instance F @(TYPE IntRep) Int = Int#

f :: forall (a :: Type). Proxy a -> F a -> F a
-- NB: Those calls to F are (F @Type a)
f _ x = x

bad = f (undefined :: Proxy Int#) 3#
