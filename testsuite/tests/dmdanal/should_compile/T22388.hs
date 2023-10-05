{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- See Note [Unboxing through unboxed tuples]
module T22388 where

-- Don't split, because neither the result not arg cancels away a box.
boring :: (# Int, Int, Int #) -> (# Int, Int, Int #)
boring (# x, y, z #) = (# y, z, x #)
{-# NOINLINE boring #-}

-- Do split, because we get to drop z and pass x and y unboxed
interesting :: (# Int, Int, Int #) -> (# Int #)
interesting (# x, y, z #) = let !t = x + y in (# t #)
{-# NOINLINE interesting #-}
