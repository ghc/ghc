{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow

handle :: ArrowPlus a => a b c -> a (b,String) c -> a b c
handle f h = proc b -> (f -< b) <+> (h -< (b,""))

f :: ArrowPlus a => a (Int,Int) String
f = proc (x,y) ->
	(|handle
		(returnA -< show y)
		(\s -> returnA -< s ++ show x)
	|)

g :: ArrowPlus a => a (Int,Int) String
g = proc (x,y) ->
	(|handle
		(\msg -> returnA -< msg ++ show y)
		(\s msg -> returnA -< s ++ show x)
	|) ("hello " ++ show x)

h :: ArrowPlus a => a (Int,Int) Int
h = proc (x,y) ->
	(
		(\z -> returnA -< x + z)
		<+>
		(\z -> returnA -< y + z)
	) (x*y)
