{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
-- | Project a witness from a known dependent package in a type.
module PackedProjection (Length, One, payload) where

import qualified PackedLength
import PackedVecLibrary

-- | The length stored alongside the vector in the constructor.
type family Length (p :: PackedVec a) :: Nat where
    Length ('PackVec n xs) = n

-- | A package containing one Boolean.
type One = 'PackVec ('S 'Z) ('VCons 'True 'VNil)

-- | Return the vector indexed by the length of this package.
payload :: foreach (p :: PackedVec a) -> Vec a (PackedLength.packageLength p)
payload (PackVec _ xs) = xs
