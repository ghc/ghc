{-# LANGUAGE MagicHash #-}

module AddMulX86 where

import GHC.Exts

f :: Int# -> Int# -> Int#
f x y =
    x +# (y *# 8#) -- Should result in a lea instruction, which we grep the assembly output for.

g x y =
    (y *# 8#) +# x  -- Should result in a lea instruction, which we grep the assembly output for.
