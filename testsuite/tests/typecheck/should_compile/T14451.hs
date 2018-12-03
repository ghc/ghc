{-# Language KindSignatures, TypeOperators, PolyKinds, TypeOperators, ConstraintKinds, TypeFamilies, DataKinds, TypeInType, GADTs, AllowAmbiguousTypes, InstanceSigs, RankNTypes, UndecidableInstances #-}
module T14451 where

import Data.Kind

data TyFun :: Type -> Type -> Type

type a ~> b = TyFun a b -> Type

type Cat ob = ob -> ob -> Type

type family
  Apply (f :: a ~> b) (x :: a) :: b where
  Apply (CompSym2 f g) a = Comp f g a

data CompSym2 :: (b ~> c) -> (a ~> b) -> (a ~> c)

type a·b = Apply a b

class Varpi (f :: i ~> j) where
  type Dom (f :: i ~> j) :: Cat i
  type Cod (f :: i ~> j) :: Cat j

  varpa :: Dom f a a' -> Cod f (f·a) (f·a')

type family
  Comp (f::k1 ~> k) (g::k2 ~> k1) (a::k2) :: k where
  Comp f g a = f · (g · a)
