{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}

-- !!! Functional dependencies
-- This broke an early impl of functional dependencies

module ShouldCompile where

class Foo r a | r -> a where
    foo :: a -> r

instance Foo (Maybe e) e where
    foo = Just

bad:: Num e => Maybe e
bad = foo 0
