{-# LANGUAGE ExplicitForAll, PolyKinds, TypeFamilies, DataKinds #-}

module T14729 where

import Data.Kind

data P k :: k -> Type

type family F a
type instance F Int = Bool

x :: forall (x :: Bool). P (F Int) x
x = undefined

y = x
