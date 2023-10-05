{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, PolyKinds, ConstraintKinds #-}

module SAKS_004 where

import Data.Kind (Type, Constraint)

type C :: (k -> Type) -> k -> Constraint
class C a b where
  f :: a b
