-- -fsplit-sections before -dynamic; split-sections-dynamic2 is the other order.
module SplitSectionsDyn (f, g) where

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g = (* 2)
