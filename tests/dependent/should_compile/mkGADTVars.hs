{-# LANGUAGE GADTs, PolyKinds, RankNTypes #-}

module GADTVars where

import Data.Kind
import Data.Proxy

type T :: Type -> forall (k2 :: Type) -> k2 -> k2 -> Type
data T k1 k2 a b where
  MkT :: T x1 Type (Proxy (y :: x1), z) z
