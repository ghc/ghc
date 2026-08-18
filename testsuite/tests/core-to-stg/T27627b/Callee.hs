{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
module Callee where

-- Not unary, so a (TC a) dictionary terminates.
class Eq a => TC a where
  tcDummy :: a -> Int

-- Unary: one superclass field, (forall a. TC (f a)).
-- See (NBD1) in Note [NON-BOTTOM-DICTS invariant].
class (forall a. TC (f a)) => UQ f

newtype Id a = MkId a
instance Eq (Id a) where _ == _ = True
instance TC (Id a) where tcDummy _ = 0
instance UQ Id

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE discard #-}
discard :: Dict c -> Int
discard _ = 42

{-# NOINLINE b #-}
b :: forall f. UQ f => f Int -> Int
b _ = discard (Dict :: Dict (Eq (f Int)))
