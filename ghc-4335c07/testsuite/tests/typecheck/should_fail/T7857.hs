module T7857 where

import Text.Printf

f :: a -> b
f = undefined

g i = f $ printf "" i
