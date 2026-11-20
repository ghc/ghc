{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# OPTIONS_GHC -O0 #-}

-- | Filter a vector without writing a package constructor or a witness.
module ImplicitVector (Filtered, filterVec, cells, forgetLength, observations, singletonBuilder, runBuilder) where

import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Nat(..), Vec(..), toList)

-- | A vector whose producer chooses its length.
type Filtered a = exists (n :: Nat). Vec a n

-- | Keep each passing element in order.
filterVec :: (a -> Bool) -> Vec a n -> Filtered a
filterVec _ VNil = VNil
filterVec p (VCons x xs)
  | p x = VCons x (filterVec p xs)
  | otherwise = filterVec p xs

-- | Open the hidden length for the call to 'toList'.
cells :: Filtered a -> [a]
cells xs = toList xs

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
runBuilder builder p x = collect builder p x
  where
    collect :: (a -> Vec a n) -> (a -> Bool) -> a -> [a]
    collect f keep y =
      let selected = filterVec keep (f y)
      in cells selected
