{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module PrimesVect (primesVect)

where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int 
import qualified Prelude

primesVect:: Int -> PArray Int
primesVect n = toPArrayP (primesVect' n)

primesVect':: Int -> [:Int:]
primesVect' n  
  | n == 1    = emptyP 
  | n == 2    = singletonP 2
  | otherwise = sps +:+ [: i | i <- enumFromToP (sq+1) n, notMultiple sps i:] 
  where

    sps = primesVect' sq
    sq =  sqrt n

    notMultiple :: [:Int:] -> Int -> Bool
    notMultiple ps i = andP [: mod i p /= 0 | p <- ps:]

