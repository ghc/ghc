{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, GADTs, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

-- | Store vectors of different lengths behind one record type.
module StoredVector (ListVector, VectorResult, Batch(..), singleton, cells, Wrapped(..), wrapVector, wrappedContents, gadtLength, nestedObservation, viewSelection) where

import GHC.Sigma (Sigma(Pair))
import GHC.Sigma.Projection (second)
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList, zipVec)

-- | A runtime length paired with a vector at that length.
type ListVector a = some (n :: Nat) | Vec a n

-- | Choose the element type while keeping the length at runtime.
type family VectorResult (keepElement :: Bool) a where
    VectorResult False _ = some (n :: Nat) | Vec Bool n
    VectorResult True a = some (n :: Nat) | Vec a n

-- | The field is strict in the pair. Its vector and length remain lazy.
data Batch a = Batch { stored :: !(some (n :: Nat) | Vec a n) }

-- | Store one cell with its runtime length.
singleton :: a -> Batch a
singleton x = Batch (Pair (S Z) (VCons x VNil))

-- | Read the vector without inspecting its stored length.
cells :: Batch a -> [a]
cells batch = let pair = stored batch in toList (second pair)

-- | Wrap a runtime length and a vector with that length.
newtype Wrapped a = Wrapped { wrapped :: some (n :: Nat) | Vec a n }

-- | Construct the pair at the supplied vector's index.
wrapVector :: foreach (n :: Nat) -> Vec a n -> Wrapped a
wrapVector n xs = Wrapped (Pair n xs)

-- | Read the length or the elements through the record selector.
wrappedContents :: Wrapped a -> (Int, [a])
wrappedContents value = case wrapped value of
    Pair n xs -> (natToInt n, toList xs)

-- | The constructor's element type scopes over the dependent field.
newtype GadtWrapped a where
    GadtWrapped :: forall b. { gadtPair :: some (n :: Nat) | Vec b n } -> GadtWrapped b

-- | Read the retained length without inspecting the vector.
gadtLength :: forall a. foreach (n :: Nat) -> Vec a n -> Int
gadtLength n xs = case gadtPair (GadtWrapped (Pair n xs) :: GadtWrapped a) of
    Pair m _ -> natToInt m

-- | Keep two vectors at the outer length and one at a separate inner length.
data NestedVectors a where
    NestedVectors :: (some (n :: Nat) |
        (Vec a n, (some (m :: Nat) | (Vec a n, Vec a m)))) -> NestedVectors a

-- | Read the lengths and first aligned cells without demanding the tails.
nestedObservation :: (Int, Int, [(Char, Char)])
nestedObservation = case sample of
    NestedVectors (Pair n (xs, Pair m (ys, _))) ->
        (natToInt n, natToInt m, take 1 (toList (zipVec xs ys)))
  where
    sample :: NestedVectors Char
    sample = build (S (S Z)) (S Z)
        (VCons 'h' (error "unused outer tail"))
        (VCons 's' (error "unused companion tail"))
        (error "unused inner vector")

    -- | Use the first length for both vectors in the aligned pair.
    build :: foreach (n :: Nat) -> foreach (m :: Nat)
          -> Vec Char n -> Vec Char n -> Vec Char m -> NestedVectors Char
    build n m xs ys zs = NestedVectors (Pair n (xs, Pair m (ys, zs)))

-- | Store a vector with its fallback and a selector usable at any length.
data Selection a where
    Selection :: foreach (n :: Nat)
              -> (foreach (n :: Nat) -> Vec a n -> (some (m :: Nat) | Vec a m))
              -> (some (m :: Nat) | (Vec a n, Vec a m))
              -> Selection a

-- | Apply the selector to a singleton and read the stored input separately.
viewSelection :: (String, String)
viewSelection = case plan of
    Selection _ select (Pair _ (input, _)) ->
        case select (S Z) (VCons 'x' VNil) of
            Pair _ chosen -> (toList input, toList chosen)
  where
    plan :: Selection Char
    plan = Selection (S (S Z)) (\n xs -> Pair n xs)
        (Pair Z (VCons 'a' (VCons 'b' VNil), VNil))
