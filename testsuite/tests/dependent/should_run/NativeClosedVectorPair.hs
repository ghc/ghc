{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeClosedVectorPair where

import IndexedVec
import Numeric.Natural (Natural)

data PairOf a (lengths :: (N, N)) where
  PairOf :: Vec n a -> Vec m a -> PairOf a '(n, m)

twoVectors :: Natural -> PairOf Natural '( 'S 'Z, 'Z)
twoVectors n = PairOf (VCons (n + 1) VNil) VNil

readPair :: PairOf Natural '( 'S 'Z, 'Z) -> Natural
readPair (PairOf (VCons x VNil) VNil) = x
