{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Combinators 
        ( foldlSU,      foldlSSU
        , foldSU,       foldSSU
        , foldl1SU,     foldl1SSU
        , fold1SU,      fold1SSU
        , foldlRU
        , combineSU)
where
import Data.Array.Parallel.Unlifted.Stream
import Data.Array.Parallel.Unlifted.Vectors                     as US
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd

-- NOTE: 
-- Even though some of these functions are trivial aliases, we use INLINE_U 
-- pragmas on them to delay inlining. This lets us see what functions are being
-- called from higher layers of the library when browsing the core IR.


-- foldl ----------------------------------------------------------------------
-- | Segmented array reduction proceeding from the left
foldlSU  :: (Unbox a, Unbox b)
         => (b -> a -> b) -> b -> USegd -> Vector a -> Vector b
foldlSU f !z segd xs 
        = unstream
        $ foldSS f z    (stream (USegd.takeLengths segd))
                        (stream xs)
{-# INLINE_U foldlSU #-}


-- | Segmented array reduction proceeding from the left.
--   For scattered segments.
foldlSSU :: (Unbox a, Unboxes a, Unbox b)
         => (b -> a -> b) -> b -> USSegd -> Vectors a -> Vector b
foldlSSU f !z ssegd xss
        = unstream
        $ foldSS f z    (stream (USSegd.takeLengths ssegd))
                        (streamSegsFromVectorsUSSegd xss ssegd)
{-# INLINE_U foldlSSU #-}


-- fold -----------------------------------------------------------------------
-- | Segmented array reduction that requires an associative combination
--   function with its unit
foldSU  :: Unbox a
        => (a -> a -> a) -> a -> USegd -> Vector a -> Vector a
foldSU = foldlSU
{-# INLINE_U foldSU #-}


-- | Segmented array reduction that requires an associative combination
--   function with its unit. For scattered segments.
foldSSU :: (Unbox a, Unboxes a)
        => (a -> a -> a) -> a -> USSegd -> Vectors a -> Vector a
foldSSU = foldlSSU       
{-# INLINE_U foldSSU #-}


-- foldl1 ---------------------------------------------------------------------
-- | Segmented array reduction from left to right with non-empty subarrays only
foldl1SU :: Unbox a
         => (a -> a -> a) -> USegd -> Vector a -> Vector a
foldl1SU f segd xs 
        = unstream
        $ fold1SS f     (stream (USegd.takeLengths segd))
                        (stream xs)
{-# INLINE_U foldl1SU #-}


-- | Segmented array reduction from left to right with non-empty subarrays only.
--   For scattered segments.
foldl1SSU :: (Unbox a, Unboxes a)
          => (a -> a -> a) -> USSegd -> Vectors a -> Vector a
foldl1SSU f ssegd xxs
        = unstream
        $ fold1SS f     (stream (USSegd.takeLengths ssegd))
                        (streamSegsFromVectorsUSSegd xxs ssegd)
{-# INLINE_U foldl1SSU #-}


-- fold1 ----------------------------------------------------------------------
-- | Segmented array reduction with non-empty subarrays and an associative
--   combination function.
fold1SU :: Unbox a
        => (a -> a -> a) -> USegd -> Vector a -> Vector a
fold1SU = foldl1SU
{-# INLINE_U fold1SU #-}


-- | Segmented array reduction with non-empty subarrays and an associative
--   combination function. For scattered segments.
fold1SSU :: (Unbox a, Unboxes a)
        => (a -> a -> a) -> USSegd -> Vectors a -> Vector a
fold1SSU = foldl1SSU
{-# INLINE_U fold1SSU #-}



-- foldlR ---------------------------------------------------------------------
-- | Regular arrar reduction 
foldlRU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
foldlRU f !z segSize
        = unstream . foldValuesR f z segSize . stream
{-# INLINE_U foldlRU #-}


-- | Merge two segmented arrays according to flag array
combineSU :: Unbox a => Vector Bool -> USegd -> Vector a -> USegd -> Vector a -> Vector a
combineSU bs xd xs yd ys
        = unstream
        $ combineSS (stream bs)
                    (stream (USegd.takeLengths xd)) (stream xs)
                    (stream (USegd.takeLengths yd)) (stream ys)
{-# INLINE_U combineSU #-}


