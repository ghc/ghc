{-# LANGUAGE DataKinds, ExplicitNamespaces, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -O0 #-}

-- | Filter a vector and hide its result length.
module ErasedVectorFilter
  ( Nat(..), Vec, Filtered, OptionalFiltered, filterVec, inspectResult
  ) where

import GHC.Exists (data Pack)

-- | A vector length.
data Nat = Z | S Nat

-- | A vector indexed by its length.
data Vec a (n :: Nat) where
  VNil :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

-- | The result exposes the elements and hides the length.
type Filtered a = exists (hidden :: Nat) | Vec a hidden

-- | An optional result still hides the length of any vector it contains.
type OptionalFiltered a = Maybe (exists (stored :: Nat) | Vec a stored)

-- | Keep the elements that satisfy the predicate, in order.
filterVec :: (a -> Bool) -> Vec a n -> exists (kept :: Nat) | Vec a kept
filterVec _ VNil = Pack (type 'Z) VNil
filterVec p (VCons x xs)
  | p x = case filterVec p xs of
      Pack (type n) ys -> Pack (type ('S n)) (VCons x ys)
  | otherwise = filterVec p xs

-- | Test whether a result contains an element.
inspectResult :: (exists (observed :: Nat) | Vec a observed) -> Bool
inspectResult xs =
  case xs of
    Pack (type n) payload -> case payload of
      VNil -> False
      VCons{} -> True
