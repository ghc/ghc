-- #4081: the strict field of T should be unboxed once, outside the blah loop,
-- not re-inspected on every iteration. See the expected Core in T4081.stderr.

module T4081 (foo) where

data T a = T !a

foo :: T Int -> Int -> Int
foo (T x) y = let blah 0 = 0
                  blah n = x + blah (n-1)
              in blah y
