{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow

h :: ArrowChoice a => Int -> a (Int,Int) Int
h x = proc (y,z) -> case compare x y of
	LT -> returnA -< x
	EQ -> returnA -< y+z
	GT -> returnA -< z+x
