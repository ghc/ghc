{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Store and consume a runtime length with an indexed vector.
module PackedVecLibrary
    ( Nat (..)
    , Vec (..)
    , PackedVec (..)
    , replicateNative
    , withPacked
    , repack
    , ignorePacked
    , filterVec
    , filterPacked
    , toList
    , natToInt
    ) where

-- | A runtime length. Each successor contains an ordinary lazy field.
data Nat = Z | S Nat

-- | A vector with an erased length parameter.
data Vec a (n :: Nat) where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

-- | Store one runtime length and a vector indexed by that same field.
data PackedVec a where
    PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a

-- | Build a vector from a runtime length.
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateNative Z x = VNil
replicateNative (S k) x = VCons x (replicateNative @a k x)

-- | Pass both stored fields to a consumer with one scoped runtime index.
withPacked :: forall a r. PackedVec a -> (foreach (n :: Nat) -> Vec a n -> r) -> r
withPacked (PackVec n xs) consume = consume n xs

-- | Reconstruct a package without evaluating either stored field.
repack :: PackedVec a -> PackedVec a
repack (PackVec n xs) = PackVec n xs

-- | Inspect the package constructor without evaluating either stored field.
ignorePacked :: PackedVec a -> Int
ignorePacked (PackVec n xs) = 42

-- | Filter a vector and store its computed length with the result.
filterVec :: forall a n. (a -> Bool) -> Vec a n -> PackedVec a
filterVec predicate VNil = PackVec Z VNil
filterVec predicate (VCons x xs) =
    if predicate x
        then case filterVec predicate xs of
            PackVec n ys -> PackVec (S n) (VCons x ys)
        else filterVec predicate xs

-- | Filter a stored vector without inspecting its stored length.
filterPacked :: forall a. (a -> Bool) -> PackedVec a -> PackedVec a
filterPacked predicate (PackVec n xs) = filterVec predicate xs

{- Note [Filtering a packed vector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's Data.Vec.filter returns Vec≤ with a hidden output length, a vector
at that length, and a bound proof. PackVec stores the first two components.
The retained branch constructs S n and VCons together. The rejected branch
returns the recursive package. See REFERENCES.md#source-vector-filter.

This implementation inspects each predicate before the recursive result.
It scans the finite input spine before returning its package. Retained
elements and stored index fields remain lazy. A bound proof and productive
filtering of infinite inputs require further work.
-}

-- | Observe a finite vector without reading its stored length.
toList :: Vec a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

-- | Observe a finite runtime length.
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n
