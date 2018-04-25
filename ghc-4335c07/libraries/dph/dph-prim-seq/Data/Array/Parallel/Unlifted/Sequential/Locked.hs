{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Locked
        ( lockedZipSU,    lockedZipWithSU
        , lockedZip3SU,   lockedZipWith3SU
        , lockedZip4SU,   lockedZipWith4SU
        , lockedZip5SU,   lockedZipWith5SU
        , lockedZip6SU,   lockedZipWith6SU
        , lockedZip7SU,   lockedZipWith7SU
        , lockedZip8SU,   lockedZipWith8SU)
where
import Data.Array.Parallel.Unlifted.Stream.Locked
import Data.Vector.Generic               as G


-- Locked Zips ----------------------------------------------------------------
-- | Zip two vectors of the same length.
--   If they do not have the same length then the result is undefined.
lockedZipSU
        :: ( Vector v a, Vector v b
           , Vector v (a, b))
        => v a -> v b
        -> v (a, b)

lockedZipSU aa bb
        = unstream $ stream2 aa bb 
{-# INLINE_U lockedZipSU #-}


-- | Zip three vectors of the same length.
lockedZip3SU
        :: ( Vector v a, Vector v b, Vector v c
           , Vector v (a, b, c))
        => v a -> v b -> v c
        -> v (a, b, c)

lockedZip3SU aa bb cc
        = unstream $ stream3 aa bb cc
{-# INLINE_U lockedZip3SU #-}


-- | Zip four vectors of the same length.
lockedZip4SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v (a, b, c, d))
        => v a -> v b -> v c -> v d
        -> v (a, b, c, d)

lockedZip4SU aa bb cc dd
        = unstream $ stream4 aa bb cc dd
{-# INLINE_U lockedZip4SU #-}


-- | Zip five vectors of the same length.
lockedZip5SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e
           , Vector v (a, b, c, d, e))
        => v a -> v b -> v c -> v d -> v e
        -> v (a, b, c, d, e)

lockedZip5SU aa bb cc dd ee
        = unstream $ stream5 aa bb cc dd ee
{-# INLINE_U lockedZip5SU #-}


-- | Zip six vectors of the same length.
lockedZip6SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f
           , Vector v (a, b, c, d, e, f))
        => v a -> v b -> v c -> v d -> v e -> v f
        -> v (a, b, c, d, e, f)

lockedZip6SU aa bb cc dd ee ff
        = unstream $ stream6 aa bb cc dd ee ff
{-# INLINE_U lockedZip6SU #-}


-- | Zip seven vectors of the same length.
lockedZip7SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g
           , Vector v (a, b, c, d, e, f, g))
        => v a -> v b -> v c -> v d -> v e -> v f -> v g
        -> v (a, b, c, d, e, f, g)

lockedZip7SU aa bb cc dd ee ff gg
        = unstream $ stream7 aa bb cc dd ee ff gg
{-# INLINE_U lockedZip7SU #-}


-- | Zip eight vectors of the same length.
lockedZip8SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v (a, b, c, d, e, f, g, h))
        => v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h
        -> v (a, b, c, d, e, f, g, h)

lockedZip8SU aa bb cc dd ee ff gg hh
        = unstream $ stream8 aa bb cc dd ee ff gg hh
{-# INLINE_U lockedZip8SU #-}


-- Locked ZipWiths ------------------------------------------------------------
lockedZipWithSU
        :: ( Vector v a, Vector v b, Vector v c
           , Vector v (a, b))
        => (a -> b -> c)
        -> v a -> v b -> v c

lockedZipWithSU f aa bb
        = G.map (\(a, b) -> f a b)
        $ lockedZipSU aa bb
{-# INLINE lockedZipWithSU #-}


lockedZipWith3SU  
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v (a, b, c))
        => (a -> b -> c -> d)
        -> v a -> v b -> v c -> v d

lockedZipWith3SU f aa bb cc
        = G.map (\(a, b, c) -> f a b c)
        $ lockedZip3SU aa bb cc
{-# INLINE lockedZipWith3SU #-}


lockedZipWith4SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e
           , Vector v (a, b, c, d))
        => (a -> b -> c -> d -> e)
        -> v a -> v b -> v c -> v d -> v e

lockedZipWith4SU f aa bb cc dd
        = G.map (\(a, b, c, d) -> f a b c d)
        $ lockedZip4SU aa bb cc dd
{-# INLINE lockedZipWith4SU #-}


lockedZipWith5SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f
           , Vector v (a, b, c, d, e))
        => (a -> b -> c -> d -> e -> f)
        -> v a -> v b -> v c -> v d -> v e -> v f

lockedZipWith5SU f aa bb cc dd ee
        = G.map (\(a, b, c, d, e) -> f a b c d e)
        $ lockedZip5SU aa bb cc dd ee
{-# INLINE lockedZipWith5SU #-}


lockedZipWith6SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g
           , Vector v (a, b, c, d, e, f))
        => (a -> b -> c -> d -> e -> f -> g)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g

lockedZipWith6SU fn aa bb cc dd ee ff
        = G.map (\(a, b, c, d, e, f) -> fn a b c d e f)
        $ lockedZip6SU aa bb cc dd ee ff
{-# INLINE lockedZipWith6SU #-}


lockedZipWith7SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v (a, b, c, d, e, f, g))
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h

lockedZipWith7SU fn aa bb cc dd ee ff gg
        = G.map (\(a, b, c, d, e, f, g) -> fn a b c d e f g)
        $ unstream $ stream7 aa bb cc dd ee ff gg
{-# INLINE lockedZipWith7SU #-}


lockedZipWith8SU
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v i
           , Vector v (a, b, c, d, e, f, g, h))
        => (a -> b -> c -> d -> e -> f -> g -> h -> i)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h -> v i

lockedZipWith8SU fn aa bb cc dd ee ff gg hh
        = G.map (\(a, b, c, d, e, f, g, h) -> fn a b c d e f g h)
        $ lockedZip8SU aa bb cc dd ee ff gg hh
{-# INLINE lockedZipWith8SU #-}


