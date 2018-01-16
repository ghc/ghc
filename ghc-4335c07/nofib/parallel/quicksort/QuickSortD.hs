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

quicksortD :: Ord a => Int -> Int -> [a] -> [a]
quicksortD _ _ [] = []
quicksortD _ _ [x] = [x]
quicksortD currentDepth limit xs | currentDepth >= limit = sort xs
quicksortD currentDepth limit (x:xs)
  = (forceList losort) `par`
    (forceList hisort) `pseq`
    losort ++ (x:hisort)
    where
    losort = quicksortD (currentDepth+1) limit [y | y <- xs, y < x]
    hisort = quicksortD (currentDepth+1) limit [y | y <- xs, y >= x]

-------------------------------------------------------------------------------

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs

-------------------------------------------------------------------------------

size :: Int
size = 1000000

depth :: Int
depth = 3

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn ("QuickSortD size=" ++ show size ++ " depth=" ++ show depth)
       let input = (take size (randomRs (0, 100000) (mkStdGen 42)))::[Integer]
       let r = quicksortD 0 depth input
       pseq r (return ())
       putStrLn ("Sum of sort: " ++ show (sum r))
