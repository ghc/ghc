{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LinearTypes #-}

module T26332 where

import Unsafe.Coerce

toLinear
  :: forall a b p q.
     (a %p-> b) %1-> (a %q-> b)
toLinear f = case unsafeEqualityProof @p @q of
  UnsafeRefl -> f
