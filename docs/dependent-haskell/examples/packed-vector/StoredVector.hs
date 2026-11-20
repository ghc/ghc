{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, GADTs, ScopedTypeVariables #-}

-- | Store vectors of different lengths behind one record type.
module StoredVector (Batch(..), singleton, cells, Wrapped(..), wrapVector, wrappedContents, gadtLength) where

import GHC.Sigma (Sigma(Pair))
import GHC.Sigma.Projection (second)
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)

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
