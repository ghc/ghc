{-# LANGUAGE DataKinds #-}

module UnsatisfiableFail3 where

import GHC.TypeError


-- This test makes sure we don't end up with duplication of error messages
-- when adding Unsatisfiable contexts to classes with superclasses.

-- Test 1: we add an Unsatisfiable context to both the class and its superclass.

class Eq a => ReflexiveEq a where
    reflexiveEq :: a -> a -> Bool
    reflexiveEq = (==)

instance Unsatisfiable (Text "Can't compare functions with (==)") => Eq (a -> b) where
  (==) = unsatisfiable

instance Unsatisfiable (Text "Can't compare functions with reflexiveEq") => ReflexiveEq (a -> b)

type DoubleMsg = Text "Equality is not reflexive on Double"
instance Unsatisfiable DoubleMsg => ReflexiveEq Double where
    reflexiveEq = unsatisfiable @DoubleMsg

foo = reflexiveEq 0 (0 :: Double)

bar = reflexiveEq (\ (x :: Int) -> x + 1)


-- Test 2: we add an Unsatisfiable context to the class, but not the superclass.

class Eq a => ReflexiveEq' a where
    reflexiveEq' :: a -> a -> Bool
    reflexiveEq' = (==)

instance Unsatisfiable (Text "Can't compare functions with reflexiveEq'") => ReflexiveEq' (a -> b)
instance Unsatisfiable DoubleMsg => ReflexiveEq' Double where
    reflexiveEq' = unsatisfiable @DoubleMsg

foo' = reflexiveEq' 0 (0 :: Double)

bar' = reflexiveEq' (\ (x :: Int) -> x + 1)
