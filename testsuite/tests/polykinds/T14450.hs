{-# Language KindSignatures, TypeOperators, PolyKinds, TypeOperators, ConstraintKinds, TypeFamilies, DataKinds, TypeInType, GADTs, AllowAmbiguousTypes, InstanceSigs #-}

module T14450 where

import Data.Kind

data TyFun :: Type -> Type -> Type

type a ~> b = TyFun a b -> Type

type Cat ob = ob -> ob -> Type

type SameKind (a :: k) (b :: k) = (() :: Constraint)

type family Apply (f :: a ~> b) (x :: a) :: b where
  Apply IddSym0 x = Idd x

class Varpi (f :: i ~> j) where
  type Dom (f :: i ~> j) :: Cat i
  type Cod (f :: i ~> j) :: Cat j

  varpa :: Dom f a a' -> Cod f (Apply f a) (Apply f a')

type family Idd (a::k) :: k where
  Idd (a::k) = a

data IddSym0 :: k ~> k where
  IddSym0KindInference :: IddSym0 l

instance Varpi (IddSym0 :: k ~> k) where
  type Dom (IddSym0 :: Type ~> Type) = (->)
