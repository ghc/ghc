module Noinline01 where
import GHC.Magic

{-# INLINE f #-}
f x = True

g = noinline f False
