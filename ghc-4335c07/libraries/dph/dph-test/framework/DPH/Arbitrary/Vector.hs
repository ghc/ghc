
-- | Generation of arbitrary vectors.
module DPH.Arbitrary.Vector where
import Test.QuickCheck
import qualified Data.Vector.Unboxed    as U
import qualified Data.Vector            as V


-- U.Vector ------------------------------------------------------------------
instance (Arbitrary a, U.Unbox a)
        => Arbitrary (U.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ U.fromList xs


-- V.Vector -------------------------------------------------------------------
instance Arbitrary a
        => Arbitrary (V.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ V.fromList xs
