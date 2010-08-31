{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise -XParallelListComp #-}
module DiophantineVect (solution3) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int

import qualified Prelude as P

solution3
 = let	pow x i		= productP (replicateP i x)
	primes 		= [: 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73 :]
	sumpri xx 	= productP [: pow p x | p <- primes | x <- xx :]
	distinct xx	= productP [: x + 1   | x <- xx :]
	
	series :: [:Int:] -> Int -> [:[:Int:]:]
	series xs n 	
	  | n == 1	= [: [: 0 :] :]
	  | otherwise	= [: [: x :] +:+ ps 
				| x <- xs
				, ps <- series (enumFromToP 0 x) (n-1) :]

	prob x y 	
 	 = let	xx	= [: (sumpri m ,m) 
				| m <- series (enumFromToP 1 3) x
				, distinct [: x * 2 | x <- m :] > y :]		
		i	= minIndexP [: a | (a, b) <- xx :]
   	   in	xx !: i	

   in	prob 7 2000

