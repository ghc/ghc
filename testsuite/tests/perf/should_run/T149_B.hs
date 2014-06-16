module Main (main) where

-- See Trac #149

-- Curently (with GHC 7.0) the CSE works, just,
-- but it's delicate.


import System.CPUTime

main :: IO ()
main = print $ playerMostOccur2 [1..m]

m :: Int
m = 22

playerMostOccur2 :: [Int] -> Int
playerMostOccur2 [a] = a
playerMostOccur2 (x:xs)
 | numOccur x (x:xs) > numOccur pmo xs = x
 | otherwise = pmo
    where pmo = playerMostOccur2 xs

numOccur :: Int -> [Int] -> Int
numOccur i is = length is

