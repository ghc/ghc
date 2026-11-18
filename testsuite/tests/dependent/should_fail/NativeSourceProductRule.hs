{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module NativeSourceProductRule where

import PackedVecLibrary (Nat(..), Vec(..), flattenRows, plus)

-- Only the type of the imported function mentions multiplication.
one :: Vec Bool ('S 'Z)
one = flattenRows (VCons (VCons True VNil) VNil)

{-# RULES "change plus" forall n m. plus n m = Z #-}
