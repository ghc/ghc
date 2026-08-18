{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
module Callee where

import Data.Kind (Constraint)

-- Reduces to (TC a), so at runtime a (UC a) dictionary is a (TC a) dictionary.
type family F a :: Constraint
type instance F a = TC a

-- Not unary.
class Eq a => TC a where
  tcDummy :: a -> Int

-- Unary: one superclass field, the unreduced (F a).
-- See (NBD1) in Note [NON-BOTTOM-DICTS invariant].
class F a => UC a

instance TC Int where tcDummy _ = 0
instance UC Int

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE discard #-}
discard :: Dict c -> Int
discard _ = 42

{-# NOINLINE b #-}
b :: forall a. UC a => a -> Int
b _ = discard (Dict :: Dict (Eq a))
