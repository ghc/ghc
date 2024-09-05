{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LinearTypes #-}

module T26332 where

import GHC.Types
import Unsafe.Coerce

toLinear
  :: forall a b (p :: Multiplicity) (q :: Multiplicity).
     (a %p-> b) %1-> (a %q-> b)
toLinear f = case unsafeEqualityProof @p @q of
  UnsafeRefl -> f
