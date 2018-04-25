{-# LANGUAGE MagicHash #-}

import GHC.Exts

f :: Float# -> Float#
f x = x
{-# NOINLINE f #-}

g :: Double# -> Double#
g x = x
{-# NOINLINE g #-}

h :: Float -> Float
h (F# x) = let a = F# (f x)
               b = D# (g (2.0##))
           in a `seq` (b `seq` a)

main = print (h 1.0)
