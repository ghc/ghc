{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ShouldFail where

class Class1 a
class Class1 a => Class2 a
class Class2 a => Class3 a

-- This was wrongfully accepted by ghc-7.0 to ghc-7.10.
-- It is missing a `Class1 a` constraint.
instance Class3 a => Class2 a
