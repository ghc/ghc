-- !!! deriving Ord without deriving Eq
--
module ShouldFail where

data Foo a b
  = C1 a Int
  | C2 b Double
  deriving Ord

