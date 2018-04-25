{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- !!! Functional dependencies
-- This broke an early impl of functional dependencies
-- (complaint about ambiguity)

module ShouldCompile where

class C a b | a -> b where f :: a -> b

g :: (C a b, Eq b) => a -> Bool
g x = f x == f x
