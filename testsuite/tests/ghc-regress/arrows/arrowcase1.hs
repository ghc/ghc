{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow

h :: ArrowChoice a => Int -> a (Int,Int) Int
h x = proc (y,z) -> case compare x y of
	LT -> returnA -< x
	EQ -> returnA -< y+z
	GT -> returnA -< z+x

g :: ArrowChoice a => Int -> a (Int,Int) Int
g x = proc (y,z) -> (case compare x y of
	LT -> \ a -> returnA -< x+a
	EQ -> \ b -> returnA -< y+z+b
	GT -> \ c -> returnA -< z+x
    ) 1
