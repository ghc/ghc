{-# LANGUAGE BangPatterns, MagicHash #-}

-- A case alternative loading the four fields of its constructor, three of which
-- a loop before the call to g uses and the fourth of which is g's argument
-- after the loop, all used again after the call: the constructor is saved
-- across the loop and the call in their place and they are loaded from it
-- again after the call (#20333).
module T20333c (Q (..), h) where

import GHC.Exts

data Q = Q !Int !Int !Int !Int | N

{-# NOINLINE g #-}
g :: Int -> Int
g x = x * 3 + 1

{-# NOINLINE h #-}
h :: Q -> Int# -> Int#
h q x = case q of
  N -> x
  Q (I# a) (I# b) (I# c) (I# d) ->
    let loop i acc | isTrue# (i >=# a) = acc
                   | otherwise = loop (i +# 1#) (acc +# (i *# b) +# c)
    in case g (I# (loop 0# x +# d)) of
         I# r -> r +# a +# b +# c +# d
