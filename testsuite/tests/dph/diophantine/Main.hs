{-# LANGUAGE ParallelArrays #-}

import Data.List
import DiophantineVect

import qualified Data.Array.Parallel.PArray   as P
import Data.Array.Parallel.Prelude


-- Solution for the 108th Euler problem.
-- From the Haskell Wiki
solution1
 = let  primes          = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
        series _ 1      = [[0]]
        series xs n     = [x:ps | x <- xs, ps <- series [0..x] (n-1) ]
        distinct        = product . map (+1)
        sumpri x        = product $ zipWith (^) primes x

        prob x y        = minimum [ (sumpri m ,m)
                                | m <- series [1..3] x
                                , (>y) $ distinct $ map (*2) m]
   in   prob 5 200

solution2
 = let  primes          = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
        series _ 1      = [[0]]
        series xs n     = [x:ps | x <- xs, ps <- series [0..x] (n-1) ]
        distinct xx     = product [ x + 1 | x <- xx ]
        sumpri xx       = product $ zipWith (^) primes xx

        prob x y        = minimum [ (sumpri m ,m)
                                        | m <- series [1..3] x
                                        , (distinct $ map (*2) m) > y ]
   in   prob 5 200


main
 = do   print solution1
        print solution2
        print solution3


