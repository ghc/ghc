{-# LANGUAGE KindSignatures, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Ctx where

import Data.Kind ( Type, Constraint )

data Proxy (ctxt :: Type -> Constraint) = Proxy

-- At one time, this one worked:
nonmeth :: ctxt Int => Proxy ctxt -> a
nonmeth prox = nonmeth prox


class Foo (ctxt :: Type -> Constraint) a where
    meth :: ctxt a => Proxy ctxt -> a

instance ctxt Int => Foo ctxt Int where
    -- But this one didn't:
    meth prox = meth prox

-- The error was:
-- Could not deduce (ctxt Int) arising from a use of `meth'
-- from the context (ctxt Int)

-- The problem was that irreducible evidence did not interact with
-- evidence of equal type.
