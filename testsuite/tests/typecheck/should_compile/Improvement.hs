{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- This one relies on doing instance reduction
-- on a /derived/ class
--    [W] C (F a0) a0, F a0 ~ Bool
-- Currently (Oct 16) I've disabled this because it seems like
-- overkill.
--
-- See Note Note [No reduction for Derived class constraints]
-- in GHC.Tc.Solver.Interact

module Foo where

type family F a
type instance F Int = Bool

class C a b where

instance (b~Int) => C Bool b

blug :: C (F a) a => a -> F a
blug = error "Urk"

foo :: Bool
foo = blug undefined
