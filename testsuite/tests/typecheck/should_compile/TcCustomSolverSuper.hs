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
-}


class (Typeable x, KnownNat x)    => C x
class (Typeable x, KnownSymbol x) => D x

instance C 2
instance D "2"

