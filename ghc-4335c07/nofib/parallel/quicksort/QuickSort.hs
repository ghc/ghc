{-# LANGUAGE BangPatterns #-}
-------------------------------------------------------------------------------
--- $Id: QuickSortD.hs#3 2008/05/27 18:27:02 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where
import System.Time
import System.Random
import Control.Parallel
import Data.List

-------------------------------------------------------------------------------

quicksortD :: Int -> Int -> [Int] -> [Int]
quicksortD _ _ [] = []
quicksortD _ _ [x] = [x]
quicksortD !currentDepth !limit xs | currentDepth >= limit = sort xs
quicksortD !currentDepth !limit (x:xs)
  = hisort `par` forceList r `pseq` r 
    where
    r = losort ++ x:hisort
    losort = quicksortD (currentDepth+1) limit [y | y <- xs, y < x]
    hisort = quicksortD (currentDepth+1) limit [y | y <- xs, y >= x]

-------------------------------------------------------------------------------

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs

-------------------------------------------------------------------------------

size :: Int
size = 500000

depth :: Int
depth = 8

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn ("QuickSortD size=" ++ show size ++ " depth=" ++ show depth)
       let input = (take size (randomRs (0, 100000) (mkStdGen 42)))::[Int]
       let r = quicksortD 0 depth input
       pseq r (return ())
       putStrLn ("Sum of sort: " ++ show (sum r))
