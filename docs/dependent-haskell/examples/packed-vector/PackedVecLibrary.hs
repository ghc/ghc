{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Store and consume a runtime length with an indexed vector.
module PackedVecLibrary (
    Nat (..),
    previous,
    isSuccessor,
    Vec (..),
    dropFirst,
    listCount,
    boolVector,
    sourceLength,
    sourceVector,
    fromListExact,
    packList,
    PackedVec (..),
    LE (..),
    BoundedVec (..),
    Add,
    plus,
    type (+),
    times,
    type (*),
    flattenRows,
    Sum (..),
    replicateNative,
    ReplicateLength,
    replicateAt,
    transportElement,
    transportElementEta,
    transportElementWith,
    transportOuterIndex,
    exposePi,
    exposePiEta,
    exposePiWith,
    exposePiOuter,
    liftExposedPi,
    withPacked,
    repack,
    ignorePacked,
    headPacked,
    unconsPacked,
    toListPacked,
    sameNat,
    zipVec,
    zipPacked,
    sumNat,
    appendVec,
    splitAtVec,
    takeVec,
    dropVec,
    CutRemainder,
    cutRemainder,
    CheckedSplit,
    splitAtVecChecked,
    appendNative,
    appendHidden,
    appendPacked,
    addSucc,
    rightZero,
    reverseAppendNative,
    reverseNative,
    reversePacked,
    proofDepthBounded,
    repackWithoutProof,
    guardedRepack,
    filterVec,
    filterPacked,
    filterBounded,
    withBounded,
    weakenRight,
    toList,
    natToInt,
    GenericCount,
    genericVector,
    genericTag,
    genericFirst,
    genericNested,
    genericRefinedNested,
) where

import Control.Exception (ArithException(Underflow), throw)
import Data.Kind (Type)
import qualified GHC.TypeNats as TN
import GHC.Sigma (Sigma (Pair))
import GHC.TypeNats.Experimental (FromNatural)
import Data.Type.Equality ((:~:) (Refl))

-- | A runtime length. Each successor contains an ordinary lazy field.
data Nat = Z | S Nat

-- | Return zero for an empty length and the predecessor for a successor.
previous :: Nat -> Nat
previous Z     = Z
previous (S n) = n

-- | Inspect the outer constructor of a length.
isSuccessor :: Nat -> Bool
isSuccessor Z     = False
isSuccessor (S _) = True

-- | A vector with an erased length parameter.
data Vec a (n :: Nat) where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

-- | Drop the first element, or return an empty vector if the input is empty.
dropFirst :: Vec a n -> Vec a (previous n)
dropFirst VNil           = VNil
dropFirst (VCons _ rest) = rest

-- | Count the spine of a Boolean list without inspecting its elements.
listCount :: [Bool] -> Nat
listCount []       = Z
listCount (_ : xs) = S (listCount xs)

-- | Preserve the elements in a vector indexed by the ordinary length function.
boolVector :: foreach (xs :: [Bool]) -> Vec Bool (listCount xs)
boolVector []       = VNil
boolVector (b : bs) = VCons b (boolVector bs)

-- | Count the spine of a list at any lifted element type.
sourceLength :: forall a. [a] -> Nat
sourceLength []       = Z
sourceLength (_ : xs) = S (sourceLength xs)

-- | Preserve each element in a vector whose index is its source list's length.
sourceVector :: forall a. foreach (xs :: [a]) -> Vec a (sourceLength xs)
sourceVector []       = VNil
sourceVector (x : xs) = VCons x (sourceVector @a xs)

-- | Store one runtime length and a vector indexed by that same field.
data PackedVec a where
    PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a

-- | Check that a list has exactly the requested length.
-- See Note [List conversion at an indexed boundary].
fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
fromListExact Z [] = Just VNil
fromListExact Z (_ : _) = Nothing
fromListExact (S _) [] = Nothing
fromListExact (S n) (x : xs) = VCons x <$> fromListExact n xs

-- | Store a list's own length with its vector. Both fields are lazy.
packList :: [a] -> PackedVec a
packList xs = PackVec (sourceLength xs) (sourceVector xs)

{- Note [List conversion at an indexed boundary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fromListExact checks the caller's length against the list spine. The
two empty constructors justify VNil. Matching both successors gives the
tail its required length. Inspect the recursive result before returning
Just. Success checks the end of the list even if the caller only needs
the head. An extra cons suffices for rejection.

packList uses the length of the input itself. sourceVector supplies a
vector at that index, so no equality test is needed. Keep both fields
lazy so that headPacked can read the first cell without counting the tail.
See docs/dependent-haskell/REFERENCES.md#checking-a-list-at-a-requested-length.
-}

-- | Evidence that the first length is at most the second length.
data LE (n :: Nat) (m :: Nat) where
    LEZ :: LE Z m
    LES :: LE n m -> LE (S n) (S m)

-- | Store a vector and evidence that its length is within the input bound.
data BoundedVec a (limit :: Nat) where
    PackBounded :: foreach (len :: Nat) -> Vec a len -> LE len limit -> BoundedVec a limit

-- | Add two lengths by inspecting the first length.
plus :: Nat -> Nat -> Nat
plus Z     m = m
plus (S n) m = S (plus n m)

-- | Natural literals and arithmetic on runtime lengths.
instance Num Nat where
    (+) = plus
    (*) = times
    n - Z = n
    Z - S _ = throw Underflow
    S n - S m = n - m
    abs n = n
    signum Z = Z
    signum (S _) = S Z
    fromInteger n
      | n < 0 = throw Underflow
      | n == 0 = Z
      | otherwise = S (fromInteger (n - 1))

-- | Display a finite length in decimal.
instance Show Nat where
    showsPrec precedence n = showsPrec precedence (natValue n :: Integer)

-- | Read a built-in natural as a Peano index. See Note [Peano numerals].
type family ToPeano (n :: TN.Nat) :: Nat where
    ToPeano 0 = 'Z
    ToPeano n = 'S (ToPeano (n TN.- 1))

type instance FromNatural Nat n = ToPeano n

-- | Construct a vector when its length is given as a type argument.
class ReplicateLength (n :: Nat) where
    -- | Put the supplied value in each cell.
    replicateKnown :: a -> Vec a n

instance ReplicateLength 'Z where
    replicateKnown _ = VNil

instance ReplicateLength n => ReplicateLength ('S n) where
    replicateKnown x = VCons x (replicateKnown x)

-- | The required type argument selects the length, as in @replicateAt 3 x@.
replicateAt :: forall (n :: Nat) -> forall a. ReplicateLength n => a -> Vec a n
replicateAt (type n) x = replicateKnown @n x

-- | Use the same addition definition in vector signatures and ordinary code.
type (n :: Nat) + (m :: Nat) = plus n m
infixl 6 type +

-- | Count the cells of a rectangular vector, one row at a time.
times :: Nat -> Nat -> Nat
times Z     m = Z
times (S n) m = plus m (times n m)

-- | Use the same multiplication for runtime lengths and vector indices.
type (n :: Nat) * (m :: Nat) = times n m
infixl 7 type *

-- | Append the rows of a rectangle. Its dimensions determine the result length.
flattenRows :: Vec (Vec a cols) rows -> Vec a (rows * cols)
flattenRows VNil = VNil
flattenRows (VCons row rest) = appendVec row (flattenRows rest)

{- Note [Peano numerals]
~~~~~~~~~~~~~~~~~~~~~~~~
The conversion from a built-in natural follows FromGHC in the fin
library. The instance selects this conversion when a type literal has
expected kind Nat. The Num instance supplies term literals. Both use
zero and successor. A Num instance alone gives GHC no such equation
in a type. The type operator and the Num method both use plus.
-}

-- | The original name for the length index of an appended vector.
type Add (n :: Nat) (m :: Nat) = plus n m

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
appendVec :: Vec a n -> Vec a m -> Vec a (n + m)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Split at the retained length. A zero cut leaves the input unevaluated.
splitAtVec :: forall a m. foreach (n :: Nat) -> Vec a (Add n m) -> (Vec a n, Vec a m)
splitAtVec Z xs = (VNil, xs)
splitAtVec (S k) (VCons x xs) = case splitAtVec k xs of
    (ys, zs) -> (VCons x ys, zs)

-- | Take the first @n@ cells. The result yields each cell before the rest.
-- Supply the suffix index @m@ explicitly when the cut is abstract.
takeVec :: forall m a. foreach (n :: Nat) -> Vec a (Add n m) -> Vec a n
takeVec Z _ = VNil
takeVec (S k) (VCons x xs) = VCons x (takeVec @m k xs)

-- | Drop the first @n@ cells without evaluating their elements.
dropVec :: forall m a. foreach (n :: Nat) -> Vec a (Add n m) -> Vec a m
dropVec n xs = snd (splitAtVec @a @m n xs)

-- | Store a prefix, a suffix at its actual length, and their length sum.
type CheckedSplit a (cut :: Nat) (total :: Nat) =
    some (suffix :: Nat) | (Vec a cut, Vec a suffix, Add cut suffix :~: total)

-- | Relate the cut and the remainder to the total.
type CutRemainder (cut :: Nat) (total :: Nat) =
    some (suffix :: Nat) | (Add cut suffix :~: total)

-- | Compute the remainder and its sum proof from the length arguments.
cutRemainder :: foreach (cut :: Nat) -> foreach (total :: Nat) -> Maybe (CutRemainder cut total)
cutRemainder cut total = case cut of
    Z ->
        let proof :: total :~: total
            proof = Refl
        in Just (Pair total proof)
    S k -> case total of
        Z -> Nothing
        S n -> case cutRemainder k n of
            Nothing -> Nothing
            Just (Pair suffix Refl) ->
                let proof :: S n :~: S n
                    proof = Refl
                in Just (Pair suffix proof)

-- | Check the cut against the total before evaluating either vector field.
splitAtVecChecked :: forall a. foreach (cut :: Nat) -> foreach (total :: Nat)
                  -> Vec a total -> Maybe (CheckedSplit a cut total)
splitAtVecChecked cut total xs = case cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix Refl) ->
        let parts :: (Vec a cut, Vec a suffix)
            parts = splitAtVec @a @suffix cut xs
        in Just (Pair suffix (fst parts, snd parts, Refl))

-- | Append vectors indexed by two retained runtime lengths.
appendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (plus n m)
appendNative n m xs ys = appendVec xs ys

-- | Append vectors whose declarations supply both retained lengths.
appendHidden :: forall a. foreach (n :: Nat). foreach (m :: Nat). Vec a n -> Vec a m -> Vec a (plus n m)
appendHidden xs ys = appendNative @a n m xs ys

-- | Compute the sum length before packaging the appended vector.
appendPacked :: PackedVec a -> PackedVec a -> PackedVec a
appendPacked (PackVec n xs) (PackVec m ys) = case sumNat n m of
    Sum p Refl -> PackVec p (appendNative n m xs ys)

{- Note [Computed runtime sum and vector append]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's builtin addition and indexed vector append recurse on the first
operand. The ordinary function plus computes the result index of appendVec.
Add names the same index for existing callers. sumNat computes a runtime p with
ordinary equality evidence p :~: Add n m. The recursive Refl branch permits
successor congruence. appendPacked matches that evidence before repackaging
the appended vector at p. The solver does not evaluate runtime indices.

The recursive witness forces the whole first length before returning Sum.
A finite first length can leave the second length and vector fields lazy.
The proof is an ordinary lifted field. Matching Refl forces it. Haskell
permits bottom and cyclic inputs; this is not a totality or erasure claim.
See docs/dependent-haskell/REFERENCES.md#computed-length-append-next-source-probe.
-}

-- | Transport addition across a successor in its second operand.
addSucc :: foreach (n :: Nat) -> foreach (m :: Nat) -> Add n (S m) :~: S (Add n m)
addSucc Z right = Refl
addSucc (S k) later = case addSucc k later of
    Refl -> Refl

-- | Prove the right identity of addition by runtime Nat induction.
rightZero :: foreach (n :: Nat) -> Add n Z :~: n
rightZero Z = Refl
rightZero (S k) = case rightZero k of
    Refl -> Refl

-- | Reverse the first vector onto the second vector.
reverseAppendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)
reverseAppendNative Z right xs acc = case xs of
    VNil -> acc
reverseAppendNative (S k) later xs acc = case xs of
    VCons x rest -> case addSucc k later of
        Refl -> reverseAppendNative @a k (S later) rest (VCons x acc)

-- | Reverse a vector while preserving its retained length.
reverseNative :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
reverseNative n xs = case rightZero n of
    Refl -> reverseAppendNative @a n Z xs VNil

-- | Keep the original stored index and delay the reversed vector.
reversePacked :: PackedVec a -> PackedVec a
reversePacked (PackVec n xs) = PackVec n (reverseNative n xs)

{- Note [Length-preserving runtime vector reverse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Agda's indexed fold and reverse provide the program precedent. This fixed-Add
worker uses the standard successor-addition and right-zero proofs explicitly.
Match each ordinary Refl proof before changing the vector index. The wrapper
stores the original runtime n. The worker type relates lengths; it does not
specify element order or establish termination.

Constructor equations share each later retained runtime Id across their rows.
All source argument lambdas precede constructor inspection, as in the case form.
The package leaves both fields lazy. Demanding the reversed vector first
demands the whole finite Nat spine through rightZero, then the vector spine.
Each addSucc can inspect the remaining Nat spine again. This is not a linear
runtime claim. Proofs are ordinary lifted values and matching Refl forces them.
See REFERENCES.md#length-preserving-reverse-next-source-probe for pinned sources.
-}

-- | Build a vector from a runtime length.
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateNative Z x = VNil
replicateNative (S k) x = VCons x (replicateNative @a k x)

-- | Transport a retained function with an outer element equality.
transportElement :: forall a b. (a ~ b) => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)
transportElement f = f

-- | Transport each application as an explicit eta control.
transportElementEta :: forall a b. (a ~ b) => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)
transportElementEta f n = f n

-- | Demand an equality proof before returning the transported function.
transportElementWith :: forall a b. (a :~: b) -> (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)
transportElementWith proof f = case proof of
    Refl -> f

-- | Transport with an equality that contains an outer runtime index.
transportOuterIndex :: forall a. foreach (k :: Nat) -> (a ~ Vec Int k) => (foreach (n :: Nat) -> (a, Vec Bool n)) -> (foreach (n :: Nat) -> (Vec Int k, Vec Bool n))
transportOuterIndex k f = f

-- | Return a retained function through an equality with its whole type.
exposePi :: forall value. (value ~ (foreach (n :: Nat) -> Vec Int n)) => value -> (foreach (n :: Nat) -> Vec Int n)
exposePi f = f

-- | Apply the transported function as an explicit eta control.
exposePiEta :: forall value. (value ~ (foreach (n :: Nat) -> Vec Int n)) => value -> (foreach (n :: Nat) -> Vec Int n)
exposePiEta f n = exposePi @value f n

-- | Demand a whole-type equality proof before returning the function.
exposePiWith :: forall value. (value :~: (foreach (n :: Nat) -> Vec Int n)) -> value -> (foreach (n :: Nat) -> Vec Int n)
exposePiWith proof f = case proof of
    Refl -> f

-- | Return a retained function whose type contains an outer runtime index.
exposePiOuter :: forall value. foreach (k :: Nat) -> (value ~ (foreach (n :: Nat) -> (Vec Int k, Vec Bool n))) => value -> (foreach (n :: Nat) -> (Vec Int k, Vec Bool n))
exposePiOuter k f = f

-- | Use a whole-type equality beneath another retained function binder.
liftExposedPi :: forall value. (value ~ (foreach (n :: Nat) -> Vec Int n)) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Vec Int n), Vec Bool m))
liftExposedPi f = f

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
headPacked (PackVec _ VNil) = Nothing
headPacked (PackVec _ (VCons x _)) = Just x

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
sameNat Z Z = Just Refl
sameNat (S left) (S right) = case sameNat left right of
    Nothing -> Nothing
    Just Refl -> Just Refl
sameNat _ _ = Nothing

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
    depth :: forall n m. LE n m -> Int
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
natToInt = natValue

-- | Count the successors using the requested numeric representation.
natValue :: Num a => Nat -> a
natValue Z = 0
natValue (S n) = 1 + natValue n

{- Note [Generic list indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GenericCount follows the list constructors. Each element keeps its identity
as an opaque value. Its type can be Int, a function type, IO Int, or Vec Int n.
The recursive tail keeps its structural list domain. These workers inspect
only the list constructors. They do not prove totality for partial or cyclic
inputs. genericFirst returns an ordinary value. Its caller controls demand.
-}

-- | Count constructors without inspecting the opaque elements.
type family GenericCount (xs :: [a]) :: Nat where
    GenericCount '[] = 'Z
    GenericCount (x ': xs) = 'S (GenericCount xs)

-- | Retain the list shape and construct a vector at its actual length.
genericVector :: forall (a :: Type). foreach (xs :: [a]) -> Vec a (GenericCount xs)
genericVector [] = VNil
genericVector (x : xs) = VCons x (genericVector @a xs)

-- | Observe only the outer constructor.
genericTag :: forall (a :: Type). foreach (xs :: [a]) -> Bool
genericTag [] = False
genericTag (_ : _) = True

-- | Return the head without demanding the tail.
genericFirst :: forall (a :: Type). foreach (xs :: [a]) -> Maybe a
genericFirst [] = Nothing
genericFirst (x : _) = Just x

-- | Instantiate the element type with an earlier runtime length.
genericNested :: foreach (n :: Nat) -> foreach (xs :: [Vec Int n]) -> Vec (Vec Int n) (GenericCount xs)
genericNested n xs = genericVector xs

-- | Refine an earlier index before checking the later list domain.
genericRefinedNested :: foreach (n :: Nat) -> foreach (xs :: [Vec Int n]) -> Vec (Vec Int n) (GenericCount xs)
genericRefinedNested Z xs = genericVector xs
genericRefinedNested (S k) xs = genericVector xs
