module T9136 where

-- In all these example, no 8 should be found in the final code
foo1 :: Int -> Int
foo1 x = (x + 8) - 1

foo2 :: Int -> Int
foo2 x = (8 + x) - 2

foo3 :: Int -> Int -> Int
foo3 x y = ((8 + x) + y) - 2

foo4 :: Int -> Int -> Int
foo4 x y = (8 + x) + (y - 3)
