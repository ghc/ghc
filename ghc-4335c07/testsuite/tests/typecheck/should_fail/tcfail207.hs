module Foo where

f :: Int -> [Int] -> [Int]
-- Want an error message that says 'take' is applied to too many args
f x = take x []

g :: [Int]
-- Want an error message that says 'take' is applied to too few args
g = take 3
