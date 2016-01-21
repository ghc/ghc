{-# LANGUAGE ImplicitParams, ConstraintKinds #-}

module T11466 where

-- This should be ok
type Bla = ?x::Int

-- This should be ook
f :: Bla => Int -> Int
f y = ?x + y

data T = T

-- But this should be rejected
instance Bla => Eq T

