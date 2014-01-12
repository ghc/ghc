import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.PArray (PArray)
import qualified Data.Array.Parallel.PArray as P

import PrimesVect (primesVect)
import Debug.Trace


primesList :: Int -> [Int]
primesList 1 = []
primesList n = sps ++ [ i | i <- [sq+1..n], multiple sps i ]
  where
    sps = primesList sq 
    sq  = floor $ sqrt $ fromIntegral n

    multiple :: [Int] -> Int -> Bool
    multiple ps i = and [i `mod` p /= 0 | p <- ps]


main 
 = do	let n			= 1000
	let resultViaNDP	= P.toList $ primesVect n
	let resultViaLists	= primesList n
	
	print resultViaNDP
	print resultViaLists
	print $ resultViaNDP == resultViaLists
	
