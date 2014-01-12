{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow

f :: Arrow a => a (Int,Int,Int) Int
f = proc (x,y,z) -> returnA -< x+y

g :: Arrow a => Int -> a Int Int
g x = proc y -> returnA -< x*y

h :: Arrow a => Int -> a (Int,Int) Int
h x = proc (y,z) -> do
	a <- f -< (x,y,3)
	b <- g (2+x) -< y+a
	returnA -< a*b+z
