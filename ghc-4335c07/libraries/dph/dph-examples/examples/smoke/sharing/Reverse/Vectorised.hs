{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (treeReversePA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int  as I
import Data.Array.Parallel.Prelude.Bool
import qualified Prelude as P


treeReversePA :: PArray Int -> PArray Int
{-# NOINLINE treeReversePA #-}
treeReversePA ps
        = toPArrayP (treeReverse (fromPArrayP ps))


-- | Reverse the elements in an array using a tree.
treeReverse :: [:Int:] -> [:Int:]
{-# NOINLINE treeReverse #-}
treeReverse xx
        | lengthP xx I.== 1
        = xx
        
        | otherwise
        = let   len     = lengthP xx
                half    = len `div` 2
                s1      = sliceP 0    half xx
                s2      = sliceP half half  xx           
          in    concatP (mapP treeReverse [: s2, s1 :])
