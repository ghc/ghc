{-# LANGUAGE UnboxedTuples #-}

module Unboxed where

data Unboxed1 = Unboxed1 (# Int, Bool #)

data Unboxed2 = Unboxed2 (# Int, (# Int, Bool #) #)

o1 = Unboxed1 (# 5, True #)
o2 = Unboxed2 (# 6, (# 7, False #) #)
