{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables #-}
module Mid where

import Ty
import {-# SOURCE #-} Callee

-- $fCATa calls Callee's $fxCBT, and that calls this one back.  The cycle runs
-- through the hs-boot import, so both dfuns are NonRec in their own module and
-- cpe_rec_ids cannot see it.  Speculating either one loops.
instance CB a => CA (T a) where
  opA _ = 1

instance CA Int where
  opA _ = 9

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE keep #-}
keep :: [Dict c] -> Int
keep xs = length xs + 41

{-# NOINLINE g #-}
g :: forall a. CB a => a -> Int
g _ = keep [Dict :: Dict (CA (T a))]
