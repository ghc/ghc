{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LinearTypes #-}

module T26332 where

import Unsafe.Coerce

-- This function should be accepted by the typechecker, and should be
-- linear-correct in the output of the desugarer, but will fail
-- -dlinear-core-lint (which does a linear-lint check after every simplifier
-- pass.  Because the optimiser discards a cast on `f` that only affects
-- linearity

toLinear
  :: forall a b p q.
     (a %p-> b) %1-> (a %q-> b)
toLinear f = case unsafeEqualityProof @p @q of
  UnsafeRefl -> f
