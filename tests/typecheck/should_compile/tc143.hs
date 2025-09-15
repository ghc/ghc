-- These two declarations get their derived instances
-- in two different ways

module ShouldCompile where

newtype Bar = Bar Int deriving Eq
data    Baz = Baz Bar deriving Eq
