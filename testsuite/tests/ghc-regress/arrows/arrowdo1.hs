{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

import Control.Arrow

f :: Arrow a => a (Int,Int,Int) (Int,Int)
f = proc (x,y,z) -> returnA -< (x,y)

g :: Arrow a => Int -> a (Int,Int) Int
g x = proc (y,z) -> returnA -< x*y

h :: Arrow a => Int -> a (Int,Int) Int
h x = proc (y,z) -> do
	(a,c) <- f -< (x,y,3)
	b <- g (2+x) -< (a,y+c)
	returnA -< a*b+z
