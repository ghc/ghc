{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
module Callee where

-- Not unary: a superclass field and a method field, so (Eq a) can be
-- speculatively selected out of a (TC a) dictionary.
class Eq a => TC a where
  tcDummy :: a -> Int

data W = W
instance Eq W where _ == _ = True

data Dict c where
  Dict :: c => Dict c

{-# NOINLINE discard #-}
discard :: Dict c -> Int
discard _ = 42

-- Never forces its dictionary, so demand analysis marks it absent, and
-- Caller's `a` inherits that absence.
{-# NOINLINE b #-}
b :: forall a. TC a => a -> Int
b _ = discard (Dict :: Dict (Eq a))
