{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoDataKinds #-}
module T22141b where

import GHC.TypeLits (Nat)
import Data.Kind (Type)

type MyNat = Nat

data Vector :: MyNat -> Type -> Type
