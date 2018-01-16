{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow

f :: Arrow a => a (Int,Int,Int) Int
f = proc (x,y,z) -> returnA -< x+y
