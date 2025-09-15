{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module TcCustomSolverSuper where

import GHC.TypeLits
import Data.Typeable

{-

When solving super-class instances, GHC solves the evidence without
using the solver (see `tcSuperClasses` in `TcInstDecls`).

However, some classes need to be excepted from this behavior,
as they have custom solving rules, and this test checks that
we got this right.

PS: this test used to have Typeable in the context too, but
    that's a redundant constraint, so I removed it

PPS: the whole structure of tcSuperClasses has changed,
     so I'm no longer sure what is being tested here
-}


class (KnownNat x)    => C x
class (KnownSymbol x) => D x

instance C 2
instance D "2"

