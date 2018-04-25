module Hi where

-- These two should be de-duplicated by CSE
f, g :: Int -> Int
f x = 2 + x
g x = 2 + x
