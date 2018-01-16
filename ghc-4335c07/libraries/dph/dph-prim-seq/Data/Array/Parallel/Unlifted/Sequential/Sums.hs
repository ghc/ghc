{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Sum-like operations on segmented list-like combinators.
module Data.Array.Parallel.Unlifted.Sequential.Sums
        ( andSU, orSU
        , sumSU, sumRU
        , productSU
        , maximumSU, minimumSU)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.USegd 
import Data.Array.Parallel.Unlifted.Sequential.Combinators


-- | Compute the boolean AND of all segments in a segmented array.
andSU :: USegd -> Vector Bool -> Vector Bool
andSU = foldSU (&&) True
{-# INLINE_U andSU #-}


-- | Compute the boolean OR of all segments in a segmented array.
orSU :: USegd -> Vector Bool -> Vector Bool
orSU = foldSU (||) False
{-# INLINE_U orSU #-}


-- | Compute the segmented sum of an array of numerals
sumSU :: (Num e, Unbox e) => USegd -> Vector e -> Vector e
sumSU = foldSU (+) 0
{-# INLINE_U sumSU #-}


-- | Compute the segmented product of an array of numerals
productSU :: (Num e, Unbox e) => USegd -> Vector e -> Vector e
productSU = foldSU (*) 1
{-# INLINE_U productSU #-}


-- | Determine the maximum element in each subarray
maximumSU :: (Ord e, Unbox e) => USegd -> Vector e -> Vector e
maximumSU = fold1SU max
{-# INLINE_U maximumSU #-}


-- | Determine the minimum element in each subarray
minimumSU :: (Ord e, Unbox e) => USegd -> Vector e -> Vector e
minimumSU = fold1SU min
{-# INLINE_U minimumSU #-}


-- | Compute the segmented sum of an array of numerals
sumRU :: (Num e, Unbox e) => Int ->Vector e -> Vector e
sumRU = foldlRU (+) 0
{-# INLINE_U sumRU #-}
