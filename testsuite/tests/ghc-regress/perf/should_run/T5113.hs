{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST

main = print (divisorCounts 1000000 ! 342)

isqrt :: Int -> Int
isqrt n = floor (sqrt $ fromIntegral n)

divisorCounts :: Int -> UArray Int Int
divisorCounts n = runSTUArray $ do
    let !rt = isqrt n
    darr <- newArray (0,n) 1 :: ST s (STUArray s Int Int)
    let inc i = unsafeRead darr i >>= \k -> unsafeWrite darr i (k+1)
        note step i
            | i > n     = return ()
            | otherwise = do
                inc i
                note step (i+step)
        count j
            | j > rt    = return ()
            | otherwise = do
                note (2*j) (j*j)
                count (j+2)
    note 2 4
    count 3
    return darr
