
{-# LANGUAGE UnboxedTuples, MagicHash #-}

module UnliftedMutVar_Comp where

import GHC.Exts

readForCAS# :: MutVar# RealWorld a -> State# RealWorld -> (# State# RealWorld, a #)
readForCAS# = unsafeCoerce# readMutVar#

  -- this used to cause a panic in boxedRepDataCon, because a levity variable
  -- was being defaulted to 'Any' instead of 'Lifted'.
