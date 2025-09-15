module Test23465 {-# WaRNING in "x-a" "b" #-} where

{-# WARNInG in "x-c" e "d" #-}
e = e

{-# WARNInG
   in "x-f" f "fw" ;
   in "x-f" g "gw"
#-}
f = f
g = g

{-# WARNinG h "hw" #-}
h = h
