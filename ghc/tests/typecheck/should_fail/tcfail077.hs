module ShouldFail where

-- !!! declaring a default method in a class that doesn't have that method.

class Foo a where
  op :: a -> a

  op2 x = x	-- Bogus declaration
