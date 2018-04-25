
-- | Generation of arbitrary nested arrays.
module DPH.Arbitrary.Array
        ( arbitraryArrayFromExp
        , AArray(..)
        , AAArray(..))
where
import Test.QuickCheck        
import Control.Monad
import DPH.Arbitrary.ArrayExp
import Data.Array.Parallel.Array                (Array)
import qualified Data.Array.Parallel.Array      as A


-------------------------------------------------------------------------------
-- | Generate an array using the given plan.
--   By using the same plan to generate two different arrays, we can ensure
--   that their internal structure is identical.
arbitraryArrayFromExp 
        :: (Array c a, Arbitrary a)
        => ArrayExp -> Gen (c a)

arbitraryArrayFromExp xx
 = case xx of
        XArbitrary n
         -> liftM A.fromList $ vector n
         
        XAppend x1 x2
         -> do  arr1    <- arbitraryArrayFromExp x1
                arr2    <- arbitraryArrayFromExp x2
                return  $ A.append arr1 arr2


-- AArray ---------------------------------------------------------------------
-- | Constrain a doubly nested array so the total number of elements contained
--   is proportional to the size parameter.
data AArray a
        = AArray a
        deriving Show


instance ( Array c1 (c2 a)
         , Arbitrary (c2 a))
        => Arbitrary (AArray (c1 (c2 a))) where

 arbitrary
  = sized $ \s -> 
  do    let s'  = truncate $ sqrt $ fromIntegral s
        xs      <- liftM A.fromList $ listOf $ resize s' $ arbitrary
        return  $ AArray xs


-- AAArray --------------------------------------------------------------------
-- | Constrain a triply nested array so the total number of elements contained
--   is proportional to the size parameter.
data AAArray a
        = AAArray a
        deriving Show

instance ( Array c1 (c2 a)
         , Arbitrary (AArray (c2 a)))
        => Arbitrary (AAArray (c1 (c2 a))) where

 arbitrary
  = sized $ \s -> 
  do    let s'  = truncate $ sqrt $ fromIntegral s
        xs      <- liftM (A.fromList . map (\(AArray a) -> a)) 
                $  listOf $ resize s' $ arbitrary
        return  $ AAArray xs
                