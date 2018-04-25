{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Basic segmented operations on unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Basics
        ( replicateSU, replicateRSU
        , appendSU
        , indicesSU, indicesSU')
where
import Data.Array.Parallel.Unlifted.Stream
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Vector.Fusion.Bundle as S


-- | Segmented replicate of a vector based on the lengths of the segments
--   of the provided `USegd`.
replicateSU :: Unbox a => USegd -> Vector a -> Vector a
replicateSU segd xs 
        = unstream
             (replicateEachS (USegd.takeElements segd)
             (S.zip (stream (USegd.takeLengths segd)) (stream xs)))
{-# INLINE_U replicateSU #-}


-- | Regular sgemented replicate.
replicateRSU :: Unbox a => Int -> Vector a -> Vector a
replicateRSU n xs
        = unstream
        . replicateEachRS n
        $ stream xs
{-# INLINE_U replicateRSU #-}
                  

-- | Segmented append.
appendSU :: Unbox a 
         => USegd -> Vector a   -- segd/data of first array
         -> USegd -> Vector a   -- segd/data of second array
         -> Vector a
appendSU xd xs yd ys
        = unstream
        $ appendSS (stream (USegd.takeLengths xd)) (stream xs)
                   (stream (USegd.takeLengths yd)) (stream ys)
{-# INLINE_U appendSU #-}


-- | Segmented indices.
indicesSU :: USegd -> Vector Int
indicesSU = indicesSU' 0
{-# INLINE_U indicesSU #-}


indicesSU' :: Int -> USegd -> Vector Int
indicesSU' i segd
        = unstream
        . indicesSS (USegd.takeElements segd) i
        . stream
        $ USegd.takeLengths segd
{-# INLINE_U indicesSU' #-}

