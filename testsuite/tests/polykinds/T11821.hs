{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs, TypeFamilies, UndecidableInstances #-}
module NotInScope where

import Data.Proxy
import Data.Kind (Type)

type KindOf (a :: k) = ('KProxy :: KProxy k)
data TyFun :: Type -> Type -> Type
type family Apply (f :: TyFun k1 k2 -> Type) (x :: k1) :: k2

data Lgo2 l1
          l2
          l3
          (l4 :: b)
          (l5 :: TyFun [a] b)
  = forall (arg :: [a]) . KindOf (Apply (Lgo2 l1 l2 l3 l4) arg) ~ KindOf (Lgo l1 l2 l3 l4 arg) =>
    Lgo2KindInference

data Lgo1 l1
          l2
          l3
          (l4 :: TyFun b (TyFun [a] b -> Type))
  = forall (arg :: b) . KindOf (Apply (Lgo1 l1 l2 l3) arg) ~ KindOf (Lgo2 l1 l2 l3 arg) =>
    Lgo1KindInference

type family Lgo f
                z0
                xs0
                (a1 :: b)
                (a2 :: [a]) :: b where
  Lgo f z0 xs0 z '[]         = z
  Lgo f z0 xs0 z ('(:) x xs) = Apply (Apply (Lgo1 f z0 xs0) (Apply (Apply f z) x)) xs
