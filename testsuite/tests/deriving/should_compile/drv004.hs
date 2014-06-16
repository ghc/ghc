-- !!! simple example of deriving Ord and Eq simultaneously
--
module ShouldSucceed where

data Foo a b c
  = C1 a Int
  | C2 b Double
  | C3 c String
  deriving (Eq, Ord)
