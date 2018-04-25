{-# LANGUAGE BangPatterns #-}

module Vector (smvm) where
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U


-- | Sparse Matrix-Vector multiplication.
smvm    :: Int
        -> V.Vector (U.Vector (Int, Double))
        -> U.Vector Double
        -> U.Vector Double

smvm _ !matrix !vector
 = let term (ix, coeff) = coeff * (vector `U.unsafeIndex` ix)
       sumRow row       = U.sum (U.map term row)
   in  U.convert $ V.map sumRow matrix
{-# NOINLINE smvm #-}
