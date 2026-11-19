{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
-- | Project a witness from a known dependent package in a type.
module PackedProjection (Length, One) where

import PackedVecLibrary

-- | The length stored alongside the vector in the constructor.
type family Length (p :: PackedVec a) :: Nat where
    Length ('PackVec n xs) = n

-- | A package containing one Boolean.
type One = 'PackVec ('S 'Z) ('VCons 'True 'VNil)
