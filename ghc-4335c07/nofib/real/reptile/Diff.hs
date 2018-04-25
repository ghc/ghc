-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Diff(diff, bcroot, square) where

square :: Int -> Int
square n = n*n

diff :: Int -> Int -> Int
diff a b = if a>b then a-b else b-a

bcroot :: Int -> Int
bcroot n = root' 0 n
	   where root' a b = if a+1>=b then b
                             else if s<n then root' m b
                             else if n<s then root' a m
                             else m
	                     where
                             m = (a+b) `div` 2
                             s = m*m



