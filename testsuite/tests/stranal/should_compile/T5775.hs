{-# LANGUAGE MagicHash, UnboxedTuples #-}
module U where
import GHC.Prim
import GHC.Types

idx :: Addr# -> Int -> Int
{-# INLINE idx #-}
idx a (I# i) = case readIntOffAddr# a i realWorld# of (# _, y #) -> I# y

f :: Int -> Int -> Int
{-# INLINE f #-}
f x y = y + x

foo :: Addr# -> Int -> Int
foo a n = n `seq` loop (idx a 0) 1
  where
    loop x i = case i >= n of
      False -> loop (f x (idx a i)) (i+1)
      True  -> x
