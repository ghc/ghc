{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

-- !!! Existentials

-- This one killed GHC 5.00.1:
--    Inferred type is less polymorphic than expected
--    Quantified type variable `a' is unified with another quantified type variable `a'
--    When checking a pattern that binds f :: a -> Int
--    In the definition of `f': f (T (x :: a) f) = T (undefined :: a) f

module Test where

data T = forall a. T a (a->Int)

f :: T -> T
f (T (x::a) f) = T (undefined::a) f
