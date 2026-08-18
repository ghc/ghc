{-# LANGUAGE GADTs, ConstraintKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
module Callee where

-- Not unary, so ($p1TC d) is not trivial and CorePrep binds it separately.
class Eq a => TC a where
  tcDummy :: a -> Int

-- Unary: at runtime a (UC a) dictionary is the (TC a) dictionary it wraps.
class TC a => UC a where {}

instance TC Int where tcDummy _ = 0
instance UC Int

data Dict c where
  Dict :: c => Dict c

-- Ignores its argument, so the Dict below is absent-demanded.
{-# NOINLINE discard #-}
discard :: Dict c -> Int
discard _ = 42

-- Body compiles to  discard (Dict @(Eq a) ($p1TC ($p1UC d)))
-- The Dict is a value, so CorePrep floats the selection out and evaluates it
-- at the head of b.  -fno-worker-wrapper keeps the dictionary parameter.
{-# NOINLINE b #-}
b :: forall a. UC a => a -> Int
b _ = discard (Dict :: Dict (Eq a))
