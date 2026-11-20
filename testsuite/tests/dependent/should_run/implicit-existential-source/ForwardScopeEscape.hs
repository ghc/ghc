module ForwardScopeEscape where

import NestedSignature
import ForwardProducer
import ForwardSchemas
import Vector

bad :: PackedPair a -> Restricted n a
bad p = choose True (keep p) (keep p)
  where
    keep :: (Vec l a, Vec r a) -> Restricted l a
    keep (ls, rs) = restrict ls rs
