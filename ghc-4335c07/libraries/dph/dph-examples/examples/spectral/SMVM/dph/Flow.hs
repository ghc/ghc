{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funfolding-use-threshold1000 #-}
module Flow (smvm) where
import Data.Vector.Unboxed
import Data.Array.Repa.Flow.Seq                 (Flow)
import qualified Data.Array.Repa.Flow.Seq       as F
import qualified Data.Vector.Unboxed            as U


-- {-# NOINLINE smvm #-}
-- smvm :: Segd U U -> Vector U (Int,Double)
--     -> Vector U Double
--     -> IO (Vector U Double)

-- smvm !segd !matrix !vector
--  = do   let (!ixs,!vals)   = R.unzip matrix
--        let  !vixs         = I.vindexs ixs vector
--        let  !vals'        = vzipWith (*) vals vixs
--        let  !res          = R.sum_s (Segd.splitSegd segd) vals'
--        return res



smvm    :: Vector Int           -- ^ Row lengths for matrix.
        -> Vector (Int, Double) -- ^ Sparse matrix column number and coefficient.
        -> Vector Double        -- ^ Dense vector.
        -> Vector Double

smvm !vLens !vMatrix !vVector
 = let  fLens   = F.flow vLens

        (vColId, vColVal)   = U.unzip vMatrix
        fColId  = F.flow vColId
        fColVal = F.flow vColVal

        fVals   = smvm' fLens fColId fColVal vVector
   in   F.unflow fVals

smvm' fLens fColId fColVal !vVector
 = let  fCoeffs     = F.gather vVector fColId 
        fVals       = F.zipWith (*) fColVal fCoeffs
   in   F.sums fLens fVals
{-# INLINE smvm' #-}
