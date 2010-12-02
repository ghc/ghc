{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module SumNatsVect (sumNats) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int 

import qualified Prelude as P

sumNats :: Int -> Int
sumNats maxN 
	= sumP [: x 	| x <- enumFromToP 0 (maxN - 1)
			, (x `mod` 3 == 0) || (x `mod` 5 == 0) :]
			
