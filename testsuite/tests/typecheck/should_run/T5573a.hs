{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Main where

import GHC.Exts

{-# NOINLINE foo1 #-} -- Make it harder to get right
foo1 x = (# x,x #)

{-# NOINLINE foo2 #-} -- Make it harder to get right
foo2 x = (# x, (# True, False #) #)

{-# NOINLINE foo3 #-} -- Make it harder to get right
foo3 (# x,y #) = x

main = print $ foo3 (# if b then x + y else x - y, 30 #)
  where (# x,  _ #) = foo1 10
        (# y, (# b, _ #) #) = foo2 20
