{-# LANGUAGE TypeFamilies #-}

module T22648b where

import Data.Kind (Type)

type D1 :: forall {k1} {k2}. k1 -> k2 -> Type
data D1 a b

type family F :: forall k1 k2. k1 -> k2 -> Type
type instance F = D1