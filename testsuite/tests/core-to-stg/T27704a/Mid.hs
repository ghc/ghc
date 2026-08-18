{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Mid where

import {-# SOURCE #-} Callee

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE keep #-}
keep :: [Dict c] -> Int
keep xs = length xs + 41

-- Builds a (UC [a]) dictionary but never forces it.  The dfun comes from the
-- hs-boot file, where UC looks non-unary, so its DFunId must not claim to
-- terminate: with -fspec-eval-dictfun, CorePrep would speculate it.
{-# NOINLINE f #-}
f :: forall a. UC a => a -> Int
f _ = keep [Dict :: Dict (UC [a])]
