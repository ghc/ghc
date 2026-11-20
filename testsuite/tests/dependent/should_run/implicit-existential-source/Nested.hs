{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, PolyKinds, TypeApplications #-}
module Nested
  ( packPair, partitionPair, localPartition, buildPair, roundTrip, hiddenKind
  , HiddenProduct, productPair, Product(..)
  ) where

import Data.Proxy (Proxy(..))
import NestedSignature
import Vector

packPair :: Vec n a -> Vec m a -> PackedPair a
packPair xs ys = (xs, ys)

partitionPair :: (a -> Bool) -> Vec n a -> PackedPair a
partitionPair _ Nil = (Nil, Nil)
partitionPair p (Cons x xs)
    | p x = yes x (partitionPair p xs)
    | otherwise = no x (partitionPair p xs)
  where
    yes :: a -> (Vec l a, Vec r a) -> PackedPair a
    yes y ~(ls, rs) = (Cons y ls, rs)
    no :: a -> (Vec l a, Vec r a) -> PackedPair a
    no y ~(ls, rs) = (ls, Cons y rs)

-- Store the whole package while the element type is still abstract.
localPartition :: (a -> Bool) -> Vec n a -> PackedPair a
localPartition p xs = let rows = partitionPair p xs in roundTrip rows

type Builder a = exists (n :: Nat). exists (m :: Nat). a -> (Vec n a, Vec m a)

buildPair :: Builder a
buildPair = \x -> (Cons x Nil, Nil)

roundTrip :: PackedPair a %1 -> PackedPair a
roundTrip p = both p
  where
    both :: (Vec l a, Vec r a) %1 -> PackedPair a
    both (ls, rs) = (ls, rs)

hiddenKind :: PackedKind
hiddenKind = Proxy @'Z

data Product a n m = Product (Vec n a) (Vec m a)

type HiddenProduct a = exists (n :: Nat). exists (m :: Nat). Product a n m

productPair :: Vec n a -> Vec m a -> HiddenProduct a
productPair ls rs = Product ls rs
