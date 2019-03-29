{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, ExplicitForAll #-}

-- See also: saks007.hs
module SAKS_007_fail where

import GHC.TypeLits (Nat)
import Data.Kind (Type, Constraint)

type family F a where { F Type = True; F _ = False }
type family G a where { G Type = False; G _ = True }

type X :: forall k1 k2. (F k1 ~ G k2) => k1 -> k2 -> Type
data X a b where
  MkX :: X Integer String  -- FAIL: F Type ~ G Type
                           --       True ~ False
