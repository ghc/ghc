{-# Language RankNTypes, TypeOperators, DataKinds, PolyKinds, GADTs, TypeFamilies #-}

module T15664 where

import Data.Kind

type family Apply (kind) (f :: kind) :: Type
data        ApplyT(kind) :: kind -> Type 

type f ~> g = (forall xx. f xx -> g xx)

unravel :: ApplyT(k) ~> Apply(k)
unravel = unravel
