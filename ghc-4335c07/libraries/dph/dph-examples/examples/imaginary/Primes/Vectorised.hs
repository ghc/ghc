{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (primesPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int  as I
import qualified Prelude

primesPA :: Int -> PArray Int
primesPA n = toPArrayP (primes n)

primes :: Int -> [:Int:]
primes n  
	| n I.== 1	= emptyP 
	| n I.== 2	= singletonP 2
	| otherwise	= sps +:+ [: i | i <- enumFromToP (sq I.+ 1) n, notMultiple sps i:] 
	where	sq	= sqrt n
		sps	= primes sq
 		
notMultiple :: [:Int:] -> Int -> Bool
notMultiple ps i = andP [: mod i p I./= 0 | p <- ps:]
