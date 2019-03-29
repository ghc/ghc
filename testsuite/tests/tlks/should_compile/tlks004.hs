{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, PolyKinds, ConstraintKinds #-}

module TLKS_004 where

import Data.Kind (Type, Constraint)

type C :: (k -> Type) -> k -> Constraint
class C a b where
  f :: a b
