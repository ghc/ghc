{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications #-}

module NativeLiteralClient where

import PackedVecLibrary (Nat, Vec, replicateNative)

three :: Vec Bool 3
three = replicateNative 3 True

twoAnnotated :: Vec Bool 2
twoAnnotated = replicateNative (2 :: Nat) True

threeConverted :: Vec Bool 3
threeConverted = replicateNative (fromInteger 3) True

twoSpecifiedConversion :: Vec Bool 2
twoSpecifiedConversion = replicateNative (fromInteger @Nat 2) True
