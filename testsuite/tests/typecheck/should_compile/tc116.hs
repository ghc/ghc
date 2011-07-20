{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
-- !!! Functional dependencies
-- This broke an early impl of functional dependencies
-- (caused a panic)

module ShouldCompile where

class Foo r a | r -> a where
    foo :: r -> a

instance Foo [m a] (m a)

bad:: Monad m => m a
bad = foo bar

bar:: [m a]
bar = []
