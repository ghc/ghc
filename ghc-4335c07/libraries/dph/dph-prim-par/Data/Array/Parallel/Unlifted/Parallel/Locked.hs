{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Locked zips and zipWiths
module Data.Array.Parallel.Unlifted.Parallel.Locked
        ( lockedZipWithUP
        , lockedZipWith3UP
        , lockedZipWith4UP
        , lockedZipWith5UP
        , lockedZipWith6UP
        , lockedZipWith7UP
        , lockedZipWith8UP)
where
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq


-- Locked ZipWiths ------------------------------------------------------------
lockedZipWithUP 
        :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c)
        -> Vector a -> Vector b 
        -> Vector c

lockedZipWithUP f as bs
        = joinD theGang balanced
        $ mapD  (What "lockedZipWithP") theGang
                (\(a, b) -> lockedZipWithSU f a b)
        $ zipD  (splitD theGang balanced as)
                (splitD theGang balanced bs)
{-# INLINE_UP lockedZipWithUP #-}


lockedZipWith3UP 
        :: (Unbox a, Unbox b, Unbox c, Unbox d)
        => (a -> b -> c -> d)
        -> Vector a -> Vector b -> Vector c
        -> Vector d

lockedZipWith3UP f as bs cs
        = joinD theGang balanced
        $ mapD  (What "lockedWith3P") theGang
                (\(a, b, c) -> lockedZipWith3SU f a b c)
        $ zip3D (splitD theGang balanced as)
                (splitD theGang balanced bs)
                (splitD theGang balanced cs)
{-# INLINE_UP lockedZipWith3UP #-}


lockedZipWith4UP 
        :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
        => (a -> b -> c -> d -> e)
        -> Vector a -> Vector b -> Vector c -> Vector d
        -> Vector e

lockedZipWith4UP f as bs cs ds
        = joinD theGang balanced
        $ mapD  (What "lockedWith4P") theGang
                (\((a, b), c, d) -> lockedZipWith4SU f a b c d)
        $ zip3D (zipD   (splitD theGang balanced as)
                        (splitD theGang balanced bs))
                (splitD theGang balanced cs)
                (splitD theGang balanced ds)
{-# INLINE_UP lockedZipWith4UP #-}


lockedZipWith5UP 
        :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
        => (a -> b -> c -> d -> e -> f)
        -> Vector a -> Vector b -> Vector c -> Vector d
        -> Vector e
        -> Vector f

lockedZipWith5UP fn as bs cs ds es
        = joinD theGang balanced
        $ mapD  (What "lockedWith5P") theGang
                (\((a, b), (c, d), e) -> lockedZipWith5SU fn a b c d e)
        $ zip3D (zipD   (splitD theGang balanced as)
                        (splitD theGang balanced bs))
                (zipD   (splitD theGang balanced cs)
                        (splitD theGang balanced ds))
                (splitD theGang balanced es)
{-# INLINE_UP lockedZipWith5UP #-}


lockedZipWith6UP 
        :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
        => (a -> b -> c -> d -> e -> f -> g)
        -> Vector a -> Vector b -> Vector c -> Vector d
        -> Vector e -> Vector f
        -> Vector g

lockedZipWith6UP fn as bs cs ds es fs
        = joinD theGang balanced
        $ mapD  (What "lockedWith6P") theGang
                (\as bs cs ds es fs -> lockedZip6SU fn)
        $ splitD theGang balanced
                (lockedZip6SU as bs cs ds es fs)
{-# INLINE_UP lockedZipWith6UP #-}

zipWith6D



lockedZipWith7UP 
        :: ( Unbox a, Unbox b, Unbox c, Unbox d
           , Unbox e, Unbox f, Unbox g, Unbox h)
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> Vector a -> Vector b -> Vector c -> Vector d
        -> Vector e -> Vector f -> Vector g
        -> Vector h

lockedZipWith7UP fn as bs cs ds es fs gs
        = joinD theGang balanced
        $ mapD  (What "lockedWith7P") theGang
                (\((a, b, c), (d, e), (f, g)) -> lockedZipWith7SU fn a b c d e f g)
        $ zip3D (zip3D  (splitD theGang balanced as)
                        (splitD theGang balanced bs)
                        (splitD theGang balanced cs))
                (zipD   (splitD theGang balanced ds)
                        (splitD theGang balanced es))
                (zipD   (splitD theGang balanced fs)
                        (splitD theGang balanced gs))
{-# INLINE_UP lockedZipWith7UP #-}


lockedZipWith8UP 
        :: ( Unbox a, Unbox b, Unbox c, Unbox d
           , Unbox e, Unbox f, Unbox g, Unbox h, Unbox i)
        => (a -> b -> c -> d -> e -> f -> g -> h -> i)
        -> Vector a -> Vector b -> Vector c -> Vector d
        -> Vector e -> Vector f -> Vector g -> Vector h
        -> Vector i

lockedZipWith8UP fn as bs cs ds es fs gs hs
        = joinD theGang balanced
        $ mapD  (What "lockedWith8P") theGang
                (\((a, b, c), (d, e), (f, g, h)) -> lockedZipWith8SU fn a b c d e f g h)
        $ zip3D (zip3D  (splitD theGang balanced as)
                        (splitD theGang balanced bs)
                        (splitD theGang balanced cs))
                (zipD   (splitD theGang balanced ds)
                        (splitD theGang balanced es))
                (zip3D  (splitD theGang balanced fs)
                        (splitD theGang balanced gs)
                        (splitD theGang balanced hs))
{-# INLINE_UP lockedZipWith8UP #-}

