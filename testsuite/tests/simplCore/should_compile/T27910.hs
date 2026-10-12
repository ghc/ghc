{-# LANGUAGE UnboxedTuples, MagicHash #-}
module T27910 where
import GHC.Exts

k1 :: (# Int, Bool #) -> (# Int, Bool #) -> Int
{-# NOINLINE k1 #-}
k1 _ _ = 3

k2 :: (# Int#, Int# #) -> (# Int#, Int# #) -> Int
{-# NOINLINE k2 #-}
k2 _ _ = 3

lifted :: Int -> Bool -> Int
lifted x y = case (# x, y #) of r -> k1 r r

unlifted :: Int# -> Int# -> Int
unlifted x y = case (# x, y #) of r -> k2 r r
