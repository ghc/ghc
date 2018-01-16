{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Basic operations on parallel unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Basics 
        ( emptyUP
        , replicateUP
        , repeatUP
        , lengthUP
        , nullUP
        , interleaveUP
        , indexedUP)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.Combinators (mapUP)
import Data.Array.Parallel.Unlifted.Parallel.Enum        (enumFromToUP)
import Data.Array.Parallel.Unlifted.Parallel.Permute     (bpermuteUP)
import GHC.Base                                           (remInt)


-- | O(1). Construct an empty array.
emptyUP :: Unbox e => Vector e
emptyUP = Seq.new 0 (const $ return ())
{-# INLINE_UP emptyUP #-}


-- | Yield an array where all elements contain the same value
replicateUP :: Unbox e => Int -> e -> Vector e
replicateUP n !e 
        = joinD theGang balanced
        . mapD  (What "replicateUP/replicate") theGang (\n' ->Seq.replicate n' e)
        $ splitLenD theGang n
{-# INLINE_UP replicateUP #-}


-- | Repeat an array the given number of times.
repeatUP :: Unbox e => Int -> Vector e -> Vector e
repeatUP n es 
        = seq m
        . bpermuteUP es
        . mapUP (\i -> i `remInt` m)
        $ enumFromToUP 0 (m*n-1)
  where
    m = Seq.length es
{-# INLINE_UP repeatUP #-}


-- | O(1). Take the length of an array.
lengthUP :: Unbox e => Vector e -> Int
lengthUP = Seq.length
{-# INLINE_UP lengthUP #-}


-- | O(1). Test whether the given array is empty
nullUP :: Unbox e => Vector e -> Bool
nullUP  = (== 0) . Seq.length
{-# INLINE_UP nullUP #-}


-- | Interleave elements of two arrays
interleaveUP :: Unbox e => Vector e -> Vector e -> Vector e
interleaveUP xs ys
        = joinD theGang unbalanced
        $ zipWithD (What "interleaveUP/interleave") theGang Seq.interleave
                (splitD theGang balanced xs)
                (splitD theGang balanced ys)
{-# INLINE_UP interleaveUP #-}


-- | Associate each element of the array with its index
indexedUP :: (DT e, Unbox e) => Vector e -> Vector (Int,e)
indexedUP 
 = splitJoinD theGang indexedFn 
 where
    sizes  arr   
        = fst 
        $ scanD (What "indexedUP/length") theGang (+) 0 $ lengthD arr

    indexedFn    
        = \arr -> zipWithD (What "indexedUP.map") theGang 
                    (\o -> Seq.map (\(x,y) -> (x + o, y)))
                    (sizes arr) 
               $  mapD     (What "indexedUP/indexed") theGang Seq.indexed arr
{-# INLINE_UP indexedUP #-}
