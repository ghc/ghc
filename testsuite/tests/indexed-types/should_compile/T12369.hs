{-# language PolyKinds, KindSignatures, GADTs, TypeFamilies, RankNTypes, TypeInType,
             TypeOperators, ConstraintKinds #-}

module T12369 where

import Data.Kind

data family Fix :: (k -> *) -> k
newtype instance Fix f = In { out :: f (Fix f) }

type FREE k = (k -> Constraint) -> (k -> k)
type f  ~> g = forall a. f a -> g a
type f ~~> g = forall a b. f a b -> g a b

data    family   Free k :: FREE k

newtype instance Free Type k p where
  Free0 :: (forall q. k q => (p  -> q) -> q) -> Free Type k p

newtype instance Free (j -> Type) k p a where
  Free1 :: (forall q. k q => (p  ~> q) -> q a) -> Free (j -> Type) k p a

newtype instance Free (j1 -> j2 -> Type) k p a b where
  Free2 :: (forall q. k q => (p ~~> q) -> q a b) -> Free (j1 -> j2 -> Type) k p a b

data family Free2 :: FREE k

newtype instance Free2 :: FREE Type where
  Free20 :: (forall q. k q => (p  -> q) -> q) -> Free2 k p

newtype instance Free2 :: forall k. FREE (k -> Type) where
  Free21 :: (forall q. k q => (p  ~> q) -> q a) -> Free2 k p a

newtype instance Free2 :: forall k1 k2. FREE (k1 -> k2 -> Type) where
  Free22 :: (forall q. k q => (p ~~> q) -> q a b) -> Free2 k p a b
