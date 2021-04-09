module Foo where

wimwam :: [a] -> Int -> Int
wimwam []     x = x
wimwam (y:ys) x = wimwam ys 0

bar xs = map (wimwam [True]) xs
