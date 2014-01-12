module Main (main) where

-- See Trac #149

-- Curently (with GHC 7.0) the CSE works, just,
-- but it's delicate.


import System.CPUTime

main :: IO ()
main = print $ playerMostOccur1 [1..m]

m :: Int
m = 22

playerMostOccur1 :: [Int] -> Int
playerMostOccur1 [a] = a
playerMostOccur1 (x:xs)
 | numOccur x (x:xs) > numOccur (playerMostOccur1 xs) xs = x
 | otherwise = playerMostOccur1 xs

numOccur :: Int -> [Int] -> Int
numOccur i is = length $ filter (i ==) is

