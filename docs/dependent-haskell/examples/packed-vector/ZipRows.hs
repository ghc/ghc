{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

module ZipRows (zipRows) where

import PackedVecLibrary (Vec(..))
import ZipLengthSource (zipLength)

-- | Pair the elements up to the end of the shorter list.
zipRows :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> Vec (a, b) (zipLength xs ys)
zipRows xs ys = case xs of
    [] -> VNil
    x : xt -> case ys of
        [] -> VNil
        y : yt -> VCons (x, y) (zipRows xt yt)
