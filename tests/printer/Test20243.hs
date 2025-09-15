{-# LANGUAGE NPlusKPatterns #-}
module Test20243 where

singleline :: Integer -> Integer
singleline (n + 1) = n

multiline :: Integer -> Integer
multiline(n
  + 1) = n

m :: Integer
(m + 1) = 3

erroR :: Int
erroR = n where
                (n+1,_) = (5,2)

g :: Int -> Int
g (x+1) = x
g y     = y
g _     = 0     -- Overlapped

h :: Int -> Int
h (x+1) = x
h _     = 0     -- Not overlapped

kh (n+2) x | x > n = x * 2
kh (x+1) (m+1) = m

takeList :: Int -> [a] -> [a]
takeList 0     _      = []
takeList (n+1) []     = []
takeList (n+1) (x:xs) = x : takeList n xs

(^^^^)          :: (Num a, Integral b) => a -> b -> a
x ^^^^ 0                =  1
x ^^^^ (n+1)    =  f x n x
                   where f _ 0 y = y
                         f x n y = g x n  where
                                   g x n | even n  = g (x*x) (n `quot` 2)
                                         | otherwise = f x (n-1) (x*y)
_ ^^^^ _                = error "(^^^^){prelude}: negative exponent"
