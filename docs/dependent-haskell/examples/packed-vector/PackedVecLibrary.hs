{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Store and consume a runtime length with an indexed vector.
module PackedVecLibrary
    ( Nat (..)
    , Vec (..)
    , PackedVec (..)
    , LE (..)
    , BoundedVec (..)
    , Add
    , Sum (..)
    , replicateNative
    , withPacked
    , repack
    , ignorePacked
    , headPacked
    , unconsPacked
    , toListPacked
    , sameNat
    , zipVec
    , zipPacked
    , sumNat
    , appendNative
    , appendPacked
    , proofDepthBounded
    , repackWithoutProof
    , guardedRepack
    , filterVec
    , filterPacked
    , filterBounded
    , withBounded
    , weakenRight
    , toList
    , natToInt
    ) where

import Data.Type.Equality ((:~:) (Refl))

-- | A runtime length. Each successor contains an ordinary lazy field.
data Nat = Z | S Nat

-- | A vector with an erased length parameter.
data Vec a (n :: Nat) where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

-- | Store one runtime length and a vector indexed by that same field.
data PackedVec a where
    PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a

-- | Evidence that the first length is at most the second length.
data LE (n :: Nat) (m :: Nat) where
    LEZ :: LE Z m
    LES :: LE n m -> LE (S n) (S m)

-- | Store a vector and evidence that its length is within the input bound.
data BoundedVec a (limit :: Nat) where
    PackBounded :: foreach (len :: Nat) -> Vec a len -> LE len limit -> BoundedVec a limit

-- | Add erased length indices by recursion on the first operand.
type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Z m = m
    Add (S n) m = S (Add n m)

-- | Store a runtime sum and evidence that it matches the erased addition.
data Sum (n :: Nat) (m :: Nat) where
    Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m

-- | Compute a runtime sum and its equality witness.
sumNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum n m
sumNat n m = case n of
    Z -> Sum m Refl
    S k -> case sumNat k m of
        Sum p Refl -> Sum (S p) Refl

-- | Append two vectors with an erased sum index.
appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Append vectors indexed by two retained runtime lengths.
appendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)
appendNative n m xs ys = appendVec xs ys

-- | Compute the sum length before packaging the appended vector.
appendPacked :: PackedVec a -> PackedVec a -> PackedVec a
appendPacked (PackVec n xs) (PackVec m ys) = case sumNat n m of
    Sum p Refl -> PackVec p (appendNative n m xs ys)

{- Note [Computed runtime sum and vector append]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's builtin addition and indexed vector append recurse on the first
operand. Add and appendVec use that shape. sumNat computes a runtime p with
ordinary equality evidence p :~: Add n m. The recursive Refl branch permits
successor congruence. appendPacked matches that evidence before repackaging
the appended vector at p. The solver does not evaluate runtime indices.

The recursive witness forces the whole first length before returning Sum.
A finite first length can leave the second length and vector fields lazy.
The proof is an ordinary lifted field. Matching Refl forces it. Haskell
permits bottom and cyclic inputs; this is not a totality or erasure claim.
See docs/dependent-haskell/REFERENCES.md#computed-length-append-next-source-probe.
-}

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

-- | Match the vector without evaluating its stored runtime length.
headPacked :: PackedVec a -> Maybe a
headPacked (PackVec n VNil) = Nothing
headPacked (PackVec m (VCons x xs)) = Just x

-- | Inspect the stored length and return the head with its packed tail.
unconsPacked :: PackedVec a -> Maybe (a, PackedVec a)
unconsPacked (PackVec Z _) = Nothing
unconsPacked (PackVec (S k) (VCons x rest)) = Just (x, PackVec k rest)

-- | Follow runtime predecessors and collect a finite package's elements.
toListPacked :: PackedVec a -> [a]
toListPacked package = case unconsPacked package of
    Nothing -> []
    Just (x, rest) -> x : toListPacked rest

-- | Compare runtime lengths and return equality evidence on success.
sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
sameNat n m = case n of
    Z -> case m of
        Z -> Just Refl
        S q -> Nothing
    S p -> case m of
        Z -> Nothing
        S q -> case sameNat p q of
            Nothing -> Nothing
            Just Refl -> Just Refl

-- | Pair elements of ordinary vectors with one shared length index.
zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil VNil = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

-- | Compare stored lengths before aligning the vector payloads.
zipPacked :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))
zipPacked (PackVec n xs) (PackVec m ys) = case sameNat n m of
    Nothing -> Nothing
    Just Refl -> Just (PackVec n (zipVec xs ys))

{- Note [Runtime length equality and vector alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sameNat compares the actual Nat values. Its successful result stores an
ordinary :~: proof. zipPacked matches Refl before using the two vector fields
at one length index. The proof variable alone supplies no local coercion.
Nothing carries no refutation. No singleton or new equality datatype is used.

The comparison inspects length constructors before any vector payload.
Equal-length success completes comparison of both finite Nat spines before
returning Just. This differs from Agda Dec's separate does and proof fields.
Matching Refl evaluates the proof. The successful package still stores a lazy
zipVec application. Its laziness concerns vectors, pair components, and tails,
not the compared indices. Finite consistent inputs have the stated alignment.
Partial and cyclic lengths need separate demand checks and can make comparison
diverge.
See REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe for the
pinned Agda equality and vector operations. Their proof erasure and totality
rules do not establish Haskell's evaluation behavior.
-}

{- Note [Uncons with a runtime predecessor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Match the stored Nat before the vector. Z returns Nothing without evaluating
the payload. S k provides the actual runtime predecessor used by PackVec.
The vector pattern checks its constructor and supplies ordinary GADT evidence
for the tail type. It does not reconstruct k from an erased type argument.

Returning Just leaves the predecessor, head, and tail fields lazy. The finite
drain follows the stored predecessors. Its functional specification assumes
finite consistent lengths and vectors. Separate bottom and cyclic checks
describe demand; they do not establish general infinite productivity.
See REFERENCES.md#source-vector-uncons for Agda's vector operations and
dependent context generalization.
-}

-- | Match and count a stored proof without evaluating the vector or length.
proofDepthBounded :: BoundedVec a limit -> Int
proofDepthBounded (PackBounded n xs LEZ) = 0
proofDepthBounded (PackBounded m ys (LES proof)) = 1 + depth proof
  where
    depth :: LE n m -> Int
    depth LEZ = 0
    depth (LES rest) = 1 + depth rest

-- | Copy the length and vector without evaluating the proof field.
repackWithoutProof :: BoundedVec a limit -> PackedVec a
repackWithoutProof (PackBounded n xs _) = PackVec n xs

-- | Reconstruct the package from the first row whose guard succeeds.
guardedRepack :: Bool -> PackedVec a -> PackedVec a
guardedRepack takeFirst (PackVec n xs)
    | takeFirst = PackVec n xs
guardedRepack takeFirst (PackVec m ys) = PackVec m ys

{- Note [Source package pattern clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each clause binds its own runtime index Id. The vector and proof patterns use
that same Id in their types. Match compilation keeps a separate constructor
case for each clause. Each case uses that clause's actual runtime fields.
Ordinary aliases do not establish equality between distinct runtime indices.

Nested constructor patterns evaluate only the selected payload or proof.
Bare and wildcard fields remain lazy. Guards and later clauses retain their
source order. Agda supplies constructor telescope and case-tree prior art;
it does not supply Haskell's evaluation rules. See
REFERENCES.md#source-package-patterns.
-}

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

-- | Pass the vector and its bound proof to a consumer at one runtime length.
withBounded :: forall a limit r. BoundedVec a limit -> (foreach (len :: Nat) -> Vec a len -> LE len limit -> r) -> r
withBounded (PackBounded len xs proof) consume = consume len xs proof

-- | Increase the upper bound by one without changing the lower bound.
weakenRight :: LE n m -> LE n (S m)
weakenRight LEZ = LEZ
weakenRight (LES proof) = LES (weakenRight proof)

-- | Filter a vector and construct evidence that its length does not increase.
filterBounded :: forall a limit. (a -> Bool) -> Vec a limit -> BoundedVec a limit
filterBounded predicate VNil = PackBounded Z VNil LEZ
filterBounded predicate (VCons x xs) =
    if predicate x
        then case filterBounded predicate xs of
            PackBounded len ys proof -> PackBounded (S len) (VCons x ys) (LES proof)
        else case filterBounded predicate xs of
            PackBounded len ys proof -> PackBounded len ys (weakenRight proof)

{- Note [Filtering with a length bound]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's Vec≤ stores a length, a vector at that length, and a bound proof.
PackBounded uses the same field relation. LEZ checks the empty case. LES
increases both proof indices when an element is retained. Right weakening
increases only the upper bound when an element is rejected.

Agda marks its bound field irrelevant. The Haskell proof is an ordinary
lazy field. Package and vector observations need not evaluate it. A bound
interpretation requires terminating indices and a terminating proof.
See REFERENCES.md#source-vector-filter for the pinned sources.
-}

{- Note [Filtering a packed vector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's Data.Vec.filter returns Vec≤ with a hidden output length, a vector
at that length, and a bound proof. PackVec stores the first two components.
The retained branch constructs S n and VCons together. The rejected branch
returns the recursive package. See REFERENCES.md#source-vector-filter.

This implementation inspects each predicate before the recursive result.
It scans the finite input spine before returning its package. Retained
elements and stored index fields remain lazy. PackVec does not store a bound
proof. See Note [Filtering with a length bound]. Productive filtering of
infinite inputs requires further work.
-}

-- | Observe a finite vector without reading its stored length.
toList :: Vec a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

-- | Observe a finite runtime length.
natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n
