-- Test module for Finite Maps

module Main where

import IO
import FiniteMap
import Util

main = 	hGetContents stdin	>>= \ input ->
       	let (s1, rest1) = rd_int input
	    r1 = test1 s1
 
	    (s2, rest2) = rd_int rest1
	    r2 = test2 s2
       	in
	putStr r1	>>
	putStr r2

rd_int = \ i -> (head (reads i)) :: (Int,String)


-------------------------------------------------------------
--Test 1 creates two big maps with the same domain, mapping
--each domain elt to 1.

test1 :: Int	 	-- Size of maps
      -> String

test1 size
  = "Test 1" 						++ "\n" ++
    "N = "			++ show size		++ "\n" ++
    "Tot sum = "		++ 
-- show (fmToList fm1) ++ show (fmToList fm2) ++ show (fmToList sum_fm) ++ 
				   show tot_sum		++ "\n" ++
    "Differences: "		++ diff			++ "\n" ++
    "Sum intersection:"		++ show sum_int		++ "\n\n"
  where
    fm1,fm2 :: FiniteMap Int Int
    fm1 = listToFM [(i,1) | i <- [1..size-1]]
    fm2 = listToFM [(i,1) | i <- [size,size-1..2]]

	-- Take their sum
    sum_fm = plusFM_C (+) fm1 fm2
    tot_sum = sum (map get [1..size])
    get n = lookupWithDefaultFM sum_fm (error ("lookup" ++ show n)) n
	-- Should be 1 + (size-2)*2 + 1 = 2*size - 2


	-- Take their difference
    diff_fm1 = fm1 `minusFM` fm2		-- Should be a singleton
    diff_fm2 = fm2 `minusFM` fm1		-- Should be a singleton
    diff     = show (fmToList diff_fm1) ++ "; " ++ show (fmToList diff_fm2)

	-- Take their intersection
    int_fm = intersectFM_C (+) fm1 fm2
    sum_int = foldFM (\k n tot -> n+tot) 0 int_fm


test2 :: Int	 	-- No of maps
      -> String

test2 size
  = "Test 2" 						++ "\n" ++
    "N = "			++ show size		++ "\n" ++
    "Sizes ="			++ show [sizeFM fm1,sizeFM fm2]	++ "\n" ++
    "Sums = "			++ show [sum1,sum2]	++ "\n\n"
  where
    fm1,fm2 :: FiniteMap Int Int

    fms1 = [unitFM i 1 | i <- [1..size]]
    fm1 = foldr (plusFM_C (+)) emptyFM fms1

    fms2 = [unitFM 1 i | i <- [1..size]]
    fm2 = foldr (plusFM_C (+)) emptyFM fms2

    sum1 = foldr (+) 0 (eltsFM fm1)
    sum2 = foldr (+) 0 (eltsFM fm2)
