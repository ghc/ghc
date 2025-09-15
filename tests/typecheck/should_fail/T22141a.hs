{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoDataKinds #-}
module T22141a where

import GHC.TypeLits (Nat)
import Data.Kind (Type)

data Vector :: Nat -> Type -> Type where
