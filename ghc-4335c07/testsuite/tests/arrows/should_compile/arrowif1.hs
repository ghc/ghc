{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow

f :: ArrowChoice a => a (Int,Int,Int) Int
f = proc (x,y,z) -> if x < y then returnA -< x+y else returnA -< x+z

g :: ArrowChoice a => Int -> a (Int,Int) Int
g x = proc (y,z) -> if x < y then returnA -< x+y else returnA -< x+z
