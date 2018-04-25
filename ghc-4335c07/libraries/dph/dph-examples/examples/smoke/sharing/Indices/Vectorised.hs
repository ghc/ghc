
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (indicesPA, indices) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int  as I
import Data.Array.Parallel.Prelude.Bool
import qualified Prelude as P


{-# NOINLINE indicesPA #-}
indicesPA :: PArray Int -> PArray Int -> PArray Int
indicesPA arr ixs
        = toPArrayP (indices (fromPArrayP arr) (fromPArrayP ixs))

indices :: [:Int:] -> [:Int:] -> [:Int:]
indices arr ixs
 = treeLookup arr ixs

{-# NOINLINE treeLookup #-}
treeLookup :: [:Int:] -> [:Int:] -> [:Int:]
treeLookup table xx
 | lengthP xx I.== 1
 = [: table !: (xx !: 0) :]
        
 | otherwise
 = let   len     = lengthP xx
         half    = len `div` 2
         s1      = sliceP 0    half xx
         s2      = sliceP half half  xx           
   in    concatP (mapP (treeLookup table) [: s1, s2 :])
