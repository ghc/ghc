
module Unboxed where
import Data.Vector.Unboxed      as U
import Stream
import qualified Generic        as G


-- Locked Zips ----------------------------------------------------------------
lockedZip
        :: (Unbox a, Unbox b)
        => Vector a -> Vector b -> Vector (a, b)

lockedZip = G.lockedZip
{-# INLINE lockedZip #-}


lockedZip3
        :: (Unbox a, Unbox b, Unbox c)
        => Vector a -> Vector b -> Vector c
        -> Vector (a, b, c)

lockedZip3 = G.lockedZip3
{-# INLINE lockedZip3 #-}


-- Locked Maps ----------------------------------------------------------------
map2    :: (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c)
        -> Vector a -> Vector b -> Vector c

map2 f aa bb
        = U.map (\(a, b) -> f a b)
        $ lockedZip aa bb
{-# INLINE map2 #-}


map3    :: (Unbox a, Unbox b, Unbox c, Unbox d)
        => (a -> b -> c -> d)
        -> Vector a -> Vector b -> Vector c -> Vector d

map3 f aa bb cc
        = U.map (\(a, b, c) -> f a b c)
        $ lockedZip3 aa bb cc
{-# INLINE map3 #-}
