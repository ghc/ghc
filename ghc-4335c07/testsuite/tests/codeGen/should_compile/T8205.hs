{-# LANGUAGE MagicHash #-}
module T8205 where

import GHC.Exts

data Stack a = Stack a (Stack a)

dropFromStack :: Int# -> Stack a -> Stack a
dropFromStack 0# l = l
dropFromStack n  (x `Stack` xs) = dropFromStack (n -# 1#) xs
