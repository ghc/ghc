{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
-- !!! Functional dependencies
-- This broke an early impl of functional dependencies
-- (complaining about ambiguity)

module ShouldCompile where

class Foo r a | r -> a where
    foo :: r -> a

instance Foo [m a] (m a)

bad:: Monad m => m a
bad = foo bar

bar:: Monad m => [m a]
bar = []
