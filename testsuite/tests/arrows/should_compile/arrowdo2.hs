{-# LANGUAGE Arrows #-}

module ShouldCompile where

import Control.Arrow

f :: Arrow a => a (Int,Int) Int
f = proc (x,y) -> do
	let z = x*y
	returnA -< y+z
