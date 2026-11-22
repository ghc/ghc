{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs, TypeOperators #-}
module NativeRetainedTailGiven where

import Data.Type.Equality (gcastWith)
import PackedVecLibrary

-- | Read the first cell at a successor length.
first :: Vec a (1 + n) -> a
first (VCons x _) = x

-- | Use the nominal Given to determine the tail of the vector.
check :: Num Nat => foreach (extent :: Nat) -> Maybe Char
check extent =
#ifdef WRONG_TAIL
    case sameNat extent Z of
        Just proof -> gcastWith proof (Just (first (replicateNative extent 'x')))
#else
    let count :: Nat
        count = 1 + extent
    in case sameNat (S Z) count of
        Just proof -> gcastWith proof (Just (first (replicateNative count 'x')))
#endif
        Nothing -> Nothing
