{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes #-}
module NativeNestedPackagePatBad where

import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider
import NativeNestedPackagePatProvider

#if defined(ESCAPE)
escape :: NestedResult a -> Vec a k
escape (NestedResult (Pair _ (_, Pair _ (_, _, whole@(VCons _ _))))) = whole
escape _ = error "empty"
#elif defined(PARTIAL)
partial :: (HiddenVec Bool, HiddenVec Bool) -> Bool
partial (VCons _ _, VCons _ _) = True
partial (VNil, VNil) = False
#else
inaccessible :: Sealed a -> Bool
inaccessible (Sealed (Coupled VNil (VCons _ _))) = True
inaccessible _ = False
#endif
