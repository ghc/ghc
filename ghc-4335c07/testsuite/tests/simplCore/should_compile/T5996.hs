module T5996 where

data C a b = C a b
a = undefined
{-# NOINLINE a #-}
b = undefined
{-# NOINLINE b #-}

x1 = C a b
x2 = C x1 b
y1 = C a b
y2 = C y1 b
