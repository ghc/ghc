{-# LANGUAGE FlexibleContexts #-}
module Generic where
import Data.Vector.Generic               as G
import Data.Vector.Generic.Base          as G
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Stream.Size    as S
import Data.Vector.Fusion.Util           as S
import Stream                            as S

{-}
lockedZipWith 
        :: (Vector v a, Vector v b, Vector v c)
        -> (a -> b -> c) -> v a -> v b -> v c

lockedZipWith f as bs 
        = unstream
        $ S.lockedZipWith f (stream as) (stream bs)
-}

-- Locked Zips ----------------------------------------------------------------
-- | Zip two vectors of the same length.
--   If they do not have the same length then the result is undefined.
lockedZip    :: (Vector v a, Vector v b, Vector v (a, b))
        => v a -> v b
        -> v (a, b)

lockedZip aa bb
        = unstream $ stream2 aa bb 
{-# INLINE lockedZip #-}


-- | Zip three vectors of the same length.
--   If they do not have the same length then the result is undefined.
lockedZip3    :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
        => v a -> v b -> v c
        -> v (a, b, c)

lockedZip3 aa bb cc
        = unstream $ stream3 aa bb cc
{-# INLINE lockedZip3 #-}


