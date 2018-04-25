
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (sumSq) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int 		as I
import Data.Array.Parallel.Prelude.Double	as D
import qualified Prelude

sumSq :: Int -> Double
{-# NOINLINE sumSq #-}
sumSq n	
 = 	D.sumP 
	(mapP (\x -> x D.* x) 
	(mapP D.fromInt 
	(enumFromToP 1 n)))