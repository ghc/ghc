{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

-- This gave "RULE left-hand side too complicated to desugar"
-- in GHC 9.8

module T24370 where

f :: (Eq a, Eq a) => a -> b -> Int
f = error "urk"

{-# SPECIALISE f :: T Maybe -> b -> Int #-}

instance (forall a. Eq a => Eq (f a)) => Eq (T f) where
  a == b = False

data T f = MkT (f Int)
