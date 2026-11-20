{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}

module NativeLiteralClient where

import PackedVecLibrary (Nat, Vec, replicateNative)

three :: Vec Bool 3
three = replicateNative 3 True

twoAnnotated :: Vec Bool 2
twoAnnotated = replicateNative (2 :: Nat) True
