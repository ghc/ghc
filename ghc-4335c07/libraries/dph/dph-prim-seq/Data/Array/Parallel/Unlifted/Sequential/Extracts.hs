{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Extracts
        ( -- * Scattered indexing.
          indexsFromVector
        , indexsFromVectorsUVSegd

          -- * Scattered extracts
        , extractsFromNestedUSSegd
        , extractsFromVectorsUSSegd
        , extractsFromVectorsUVSegd
        , extractsFromVectorsUSSegdSegmap)
where
import Data.Array.Parallel.Unlifted.Stream                      as US
import Data.Array.Parallel.Unlifted.Vectors                     as US
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.UVSegd           (UVSegd)
import qualified Data.Vector                                    as V


-- Indexs --------------------------------------------------------------------
-- | Lookup elements from a `Vector`.
indexsFromVector
        :: Unbox a
        => Vector a -> Vector Int -> Vector a

indexsFromVector vector ixs
        = U.unstream 
        $ streamElemsFromVector vector 
        $ U.stream ixs
{-# INLINE_U indexsFromVector #-}


-- | Lookup elements from some `Vectors` through a `UPVSegd`.
indexsFromVectorsUVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsUVSegd vectors uvsegd vsrcixs
        = U.unstream 
        $ streamElemsFromVectorsVSegd vectors uvsegd 
        $ U.stream vsrcixs
{-# INLINE_U indexsFromVectorsUVSegd #-}


-- Extracts wrappers ---------------------------------------------------------
-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromNestedUSSegd
        :: (U.Unbox a)
        => USSegd -> V.Vector (Vector a) -> U.Vector a

extractsFromNestedUSSegd ussegd vectors
        = U.unstream $ streamSegsFromNestedUSSegd vectors ussegd
{-# INLINE_U extractsFromNestedUSSegd #-}


-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromVectorsUSSegd
        :: (Unboxes a, U.Unbox a)
        => USSegd -> Vectors a -> U.Vector a

extractsFromVectorsUSSegd ussegd vectors
        = U.unstream $ streamSegsFromVectorsUSSegd vectors ussegd
{-# INLINE_U extractsFromVectorsUSSegd #-}


-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromVectorsUVSegd
        :: (Unbox a, US.Unboxes a)
        => UVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUVSegd uvsegd vectors
        = U.unstream  $ US.streamSegsFromVectorsUVSegd vectors uvsegd
{-# INLINE_U extractsFromVectorsUVSegd #-}


-- | Copy segments defined by a segmap and `USSegd` into a new array.
extractsFromVectorsUSSegdSegmap
        :: (Unbox a, US.Unboxes a)
        => USSegd
        -> Vector  Int
        -> Vectors a
        -> Vector  a

extractsFromVectorsUSSegdSegmap ussegd segmap vectors
        = U.unstream $ US.streamSegsFromVectorsUSSegdSegmap vectors ussegd segmap

