{-# LANGUAGE GADTs, RankNTypes, PolyKinds, TypeApplications,
             TypeAbstractions, ScopedTypeVariables, TypeFamilies, DataKinds #-}

module T18986b where

import Data.Kind (Type)
import Data.Type.Equality
import Data.Proxy

type T :: (forall k. k -> Type) -> k1 -> k2 -> Type
data T f a b where
  MkT :: forall (f :: forall k. k -> Type) a b.
    f a -> f b -> T f a b

type family Id a where
  Id a = a

k1 :: forall (f :: forall k. k -> Type). T f Int Maybe -> T f Either True -> f :~: f
k1
  (MkT @(f :: forall k . k -> Type) (x :: f Int) (y :: f Maybe))
  (MkT @(g :: forall k . k -> Type) (a :: f Either) (b :: g True))
  = Refl :: f :~: g

k2 :: forall (f :: forall k. k -> Type). T f Int Maybe -> T f Either True -> f :~: f
k2
  (MkT @f (x :: f Int) (y :: f Maybe))
  (MkT @g (a :: f Either) (b :: g True))
  = Refl :: f :~: g

k3 :: forall (f :: forall k. k -> Type). T f Int Maybe -> T f Either True -> f :~: f
k3
  (MkT @(f :: forall k . k -> Type) (x :: Id f (Id Int)) (y :: Id f (Id Maybe)))
  (MkT @g (a :: Id f (Id Either)) (b :: Id g (Id True)))
  = Refl :: Id f :~: Id g

k4 :: T Proxy Int Maybe -> T Proxy Either True -> Proxy :~: Proxy
k4
  (MkT @f (x :: f Int) (y :: f Maybe))
  (MkT @g (a :: f Either) (b :: g True))
  = Refl :: f :~: g
