{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import GHC.TypeError

-- This test makes sure we don't end up with duplication of error messages
-- when adding Unsatisfiable contexts to classes with superclasses.

-- Test 1: we add an Unsatisfiable context to both the class and its superclass.

class ReflexiveEq a where
    reflexiveEq :: a -> a -> Bool

type DoubleMsg = Text "Equality is not reflexive on Double"
instance Unsatisfiable DoubleMsg => ReflexiveEq Double

foo = reflexiveEq 0 (0 :: Double)

main :: IO ()
main = print foo
