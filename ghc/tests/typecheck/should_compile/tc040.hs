module ShouldSucceed where

-- !!! tests the deduction of contexts.

f :: (Eq a) => a -> [a]

f x = g x
      where
      g y = if (y == x) then [] else [y]
