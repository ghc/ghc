{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Basic operations on distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Basics 
        (eqD, neqD, toD, fromD)
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Distributed.Combinators 
import Data.Array.Parallel.Unlifted.Distributed.Data.Bool
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar
import Control.Monad ( zipWithM_ )

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Basics." ++ s


-- | Test whether to distributed values are equal. 
--   This requires a 'Gang' and hence can't be defined in terms of 'Eq'.
eqD :: (Eq a, DT a) => Gang -> Dist a -> Dist a -> Bool
eqD g dx dy 
        = andD g (zipWithD (What "eq") g (==) dx dy)


-- | Test whether to distributed values are not equal.
--   This requires a 'Gang' and hence can't be defined in terms of 'Eq'.
neqD :: (Eq a, DT a) => Gang -> Dist a -> Dist a -> Bool
neqD g dx dy 
        = orD g (zipWithD (What "neq") g (/=) dx dy)


-- | Generate a distributed value from the first @p@ elements of a list.
-- 
--   * For debugging only, don't use in production code.
toD :: DT a => Gang -> [a] -> Dist a
toD g xs
        = newD g (\md -> zipWithM_ (writeMD md) [0 .. gangSize g - 1] xs)


-- | Yield all elements of a distributed value.
--
--   * For debugging only, don't use in production code.
fromD :: DT a => Gang -> Dist a -> [a]
fromD g dt 
        = checkGangD (here "fromDT") g dt 
        $ map   (indexD (here "fromD") dt) 
                [0 .. gangSize g - 1]

