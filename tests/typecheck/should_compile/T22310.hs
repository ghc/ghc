{-# LANGUAGE GADTs, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module T22310 where

import Data.Coerce ( coerce )
import Data.Kind   ( Type )

type Some :: (Type -> Type) -> Type
data Some t where
  Some :: t ex -> Some t

type NT :: Type -> Type
newtype NT f = MkNT ()

oops :: Some NT -> Some NT
oops = coerce (\(Some @NT x) -> Some x)
  -- After canonicalisation of Wanted constraints,
  -- we end up with:
  --
  -- [W] t[tau:0] ~R# NT
  --
  -- Note the newtype TyCon on the RHS.
  -- Does not violate TyEq:N, because it is unsaturated!
