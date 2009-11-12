{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fvia-C -optc-O3 -fexcess-precision -optc-msse3 #-}

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base

main = print $ runST
          (do arr <- newArray (1,2000000) 137.0 :: ST s (STUArray s Int Double)
              go arr 2000000 0.0 )


go :: STUArray s Int Double -> Int -> Double -> ST s Double
go !a i !acc
    | i < 1     = return acc
    | otherwise = do
         b <- unsafeRead a i
         unsafeWrite a i (b+3.0)
         c <- unsafeRead a i
         go a (i-1) (c+acc)
