{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow

f :: Arrow a => a (Int,Int) Int
f = proc (x,y) -> let z = x*y in returnA -< y+z
