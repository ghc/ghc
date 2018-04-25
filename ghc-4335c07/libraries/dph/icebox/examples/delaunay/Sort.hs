{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Sort ( medianIndex, collect, sort ) where

import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int as I

import qualified Prelude as P

kthSmallestIndex :: [:(Int,D.Double):] -> Int -> Int
kthSmallestIndex xs k
  | k >= lengthP lts && k < n - lengthP gts = i
  | otherwise = (kthSmallestIndex ys k')
  where
    n     = lengthP xs
    (i,x) = xs !: (n `div` 2)

    lts   = [:(j,y) | (j,y) <- xs, y D.< x:]
    gts   = [:(j,y) | (j,y) <- xs, y D.> x:]

    (ys, k') | k < lengthP lts = (lts, k)
             | otherwise       = (gts, k - (n - lengthP gts))

medianIndex :: [:D.Double:] -> Int
medianIndex xs = (kthSmallestIndex (indexedP xs) (lengthP xs `div` 2))

collect :: [:(Int,Int):] -> [:(Int,[:Int:]):]
collect ps
  | lengthP ps I.== 0 = [::]
  | otherwise = (
  let
    (pivot,_) = ps !: (lengthP ps `I.div` 2)
    ls        = [:(i,j) | (i,j) <- ps, i I.< pivot:]
    gs        = [:(i,j) | (i,j) <- ps, i I.> pivot:]
    js        = [:j | (i,j) <- ps, i I.== pivot:]

    ss        = mapP collect [:ls,gs:]
  in
  (ss!:0) +:+ [:(pivot,sort js):] +:+ (ss!:1)
  )

sort :: [:Int:] -> [:Int:]
sort xs | lengthP xs I.<= 1 = xs
sort xs = ((ss!:0) +:+ [:pivot:] +:+ (ss!:1))
  where
    pivot = xs !: (lengthP xs `I.div` 2)
    ls    = [:x | x <- xs, x < pivot:]
    gs    = [:x | x <- xs, x > pivot:]
    ss    = mapP sort [:ls,gs:]

