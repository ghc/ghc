{-# LANGUAGE MagicHash #-}

module T24662 where

import GHC.Exts

f1 :: a -> Int# -> Int -> Int
{-# OPAQUE f1 #-}
f1 _ x (I# y) = I# (x +# y)

f2 :: Int# -> a -> Int -> Int
{-# OPAQUE f2 #-}
f2 x _ (I# y) = I# (x +# y)

loopy :: Int -> Int#
loopy x | x>0       = loopy x
        | otherwise = 0#

-- Should either let or case-bind t (preferrably the latter), but we should do
-- it consistently in foo1 and foo2.
foo1 x = let t :: Int -> Int
             t = f1 True (loopy x) in
         t `seq` (x, t)

foo2 x = let t :: Int -> Int
             t = f2 (loopy x) True in
         t `seq` (x, t)
