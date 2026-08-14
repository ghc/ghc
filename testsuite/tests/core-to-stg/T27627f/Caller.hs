{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Caller where

import Callee
import Data.Kind (Constraint)

-- A Constraint-kinded type family.  It reduces to (TC W), so the dictionary
-- `a` receives really is a (TC W) dictionary; but (F W) is not class-headed,
-- so isDictTy says False.  Worker/wrapper must still not replace it with a
-- filler: Callee speculates a superclass selection out of it.
type family F a :: Constraint
type instance F W = TC W

-- Note there is no `instance TC W` in scope here: `b W` must take its
-- dictionary from this Given, rather than from an instance.
{-# NOINLINE a #-}
a :: F W => Int -> Int
a x = b W + x
