--!!! simple example of deriving Ord (and, implicitly, Eq)
--
data Foo a b
  = C1 a Int
  | C2 b Double
  deriving Ord
