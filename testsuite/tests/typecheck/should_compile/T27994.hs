{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module  T27994 (l2) where

import Data.Kind (Constraint, Type)

l1 :: [String]
l1 = ["hello", "world!"]

l2 :: [Int]
l2 = gmap (HCons length HNil) l1

-----

type HList :: [Type] -> Type
data HList p where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x:xs)

type ($$) :: k -> [Type] -> Type
type family s $$ p where
  (s :: Type)      $$ '[]   = s
  (s :: Type -> k) $$ (a:b) = s a $$ b

type Doms :: [Type] -> [Type]
type family Doms t where
  Doms '[]            = '[]
  Doms ((a -> b) : t) = a : Doms t

type Codoms :: [Type] -> [Type]
type family Codoms t where
  Codoms '[]            = '[]
  Codoms ((a -> b) : t) = b : Codoms t

type GMap :: k -> [Type] -> Constraint
class GMap (s :: k) t | t -> k where
  gmap :: HList t -> s $$ Doms t -> s $$ Codoms t

instance GMap [] '[a -> b] where
  gmap :: HList '[a -> b] -> [a] -> [b]
  gmap (HCons f HNil) = go
    where
      go :: [a] -> [b]
      go []     = []
      go (x:xs) = f x : go xs
