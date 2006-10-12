
module Main (main) where

import System.CPUTime

main = do start <- getCPUTime
          putStrLn "Start"
          print $ playerMostOccur1 [1..m]
          middle <- getCPUTime
          putStrLn "Middle"
          print $ playerMostOccur2 [1..m]
          end <- getCPUTime
          putStrLn "End"
          let d1 = middle - start
              d2 = end - middle
          if d1 > 2 * d2
            then do print d1
                    print d2
            else putStrLn "OK!"

m :: Int
m = 22

playerMostOccur1 :: [Int] -> Int
playerMostOccur1 [a] = a
playerMostOccur1 (x:xs)
 | numOccur x (x:xs) > numOccur (playerMostOccur1 xs) xs = x
 | otherwise = playerMostOccur1 xs

playerMostOccur2 :: [Int] -> Int
playerMostOccur2 [a] = a
playerMostOccur2 (x:xs)
 | numOccur x (x:xs) > numOccur pmo xs = x
 | otherwise = pmo
    where pmo = playerMostOccur2 xs

numOccur :: Int -> [Int] -> Int
numOccur i is = length $ filter (i ==) is

