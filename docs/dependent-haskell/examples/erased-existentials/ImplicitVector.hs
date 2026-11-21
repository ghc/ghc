{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, TypeOperators #-}

-- | Filter a vector without writing a package constructor or a witness.
module ImplicitVector (Filtered, FilteredWith, filterVec, withVector, partitionVec, firstKept, partitionRows, rowLists, firstPassing, cells, transportFiltered, pairWithSelf, forgetLength, observations, singletonBuilder, runBuilder) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Nat(..), Vec(..), toList, zipVec)

-- | A vector whose producer chooses its length.
type Filtered a = exists (n :: Nat). Vec a n

-- | A vector and a reader of that same vector length.
type FilteredWith a r = exists (n :: Nat). (Vec a n, Vec a n -> r)

-- | Keep each passing element in order.
filterVec :: (a -> Bool) -> Vec a n -> Filtered a
filterVec _ VNil = VNil
filterVec p (VCons x xs)
  | p x = VCons x (filterVec p xs)
  | otherwise = filterVec p xs

-- | Apply a consumer that works for every vector length.
withVector :: (forall k. Vec a k -> r) -> Vec a n -> r
withVector use xs = use xs

-- | Change the element type using a proof under the same hidden length.
transportFiltered :: a :~: b -> Filtered a -> Filtered b
transportFiltered Refl xs = xs

-- | Split the vector. Each result has its own hidden length.
partitionVec :: (a -> Bool) -> Vec a n -> (Filtered a, Filtered a)
partitionVec _ VNil = (VNil, VNil)
partitionVec p (VCons x xs) =
  let (yes, no) = partitionVec p xs
  in if p x then (VCons x yes, no) else (yes, VCons x no)

-- | Read the first kept element without opening the rejected partition.
firstKept :: (Filtered a, Filtered a) -> Maybe a
firstKept (VNil, _) = Nothing
firstKept (VCons x _, _) = Just x

-- | Store both vectors in one payload.
data Rows a n m = Rows (Vec a n) (Vec a m)

-- | Erase the two independent lengths in binder order.
type HiddenRows a = exists (n :: Nat). exists (m :: Nat). Rows a n m

-- | Collect both outputs in one package.
partitionRows :: (a -> Bool) -> Vec a n -> HiddenRows a
partitionRows p xs =
  let (yes, no) = partitionVec p xs
  in Rows yes no

-- | Match the payload constructor through both existential binders.
rowLists :: HiddenRows a -> ([a], [a])
rowLists rows = case rows of
  Rows left right -> (toList left, toList right)

-- | Read only the first cell from the passing side of a partition.
firstPassing :: HiddenRows a -> Maybe a
firstPassing (Rows VNil _) = Nothing
firstPassing (Rows (VCons x _) _) = Just x

-- | Open the hidden length for the call to 'toList'.
cells :: Filtered a -> [a]
cells xs = toList xs

-- | Zip two uses of the same vector at its hidden length.
pairWithSelf :: Filtered a -> [(a, a)]
pairWithSelf xs = pairs xs xs
  where
    pairs :: Vec a n -> Vec b n -> [(a, b)]
    pairs left right = toList (zipVec left right)

-- | Discard the stored length and keep the vector at the same index.
forgetLength :: (some (n :: Nat) | Vec a n) -> Filtered a
forgetLength (Pair _ xs) = xs

-- | Observe a lazy tail, an empty result, and a retained input.
observations :: ([Int], [Int], [Int])
observations =
  ( take 1 (cells (filterVec odd (VCons 7 (error "unseen tail"))))
  , cells (filterVec odd (VCons 2 VNil))
  , cells (forgetLength (Pair (S Z) (VCons 9 VNil)))
  )

-- | The builder chooses one length for all of its inputs.
type Builder a = exists (n :: Nat). a -> Vec a n

-- | Package a lambda that always returns one element.
singletonBuilder :: Builder a
singletonBuilder = \x -> VCons x VNil

-- | Open the builder, then filter its output at a new hidden length.
runBuilder :: Builder a -> (a -> Bool) -> a -> [a]
runBuilder builder p x =
  let selected = filterVec p (builder x)
  in cells selected
