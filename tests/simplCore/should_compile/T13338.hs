{-# LANGUAGE MagicHash #-}

module T13338 where

import GHC.Exts

magic# :: Int# -> Bool
magic# x# = True
{-# NOINLINE magic# #-}

f :: Int# -> Int -> Int
f x# n = length [ i | i@(I# i#) <- [0..n], magic# (remInt# x# 100000# -# i#) ]
