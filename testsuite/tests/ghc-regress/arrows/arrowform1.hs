{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

import Control.Arrow

handle :: ArrowPlus a => a b c -> a (b,String) c -> a b c
handle f h = proc b -> (f -< b) <+> (h -< (b,""))

f :: ArrowPlus a => a (Int,Int) String
f = proc (x,y) ->
	(|handle|) (returnA -< show y) (\s -> returnA -< s ++ show x)

inject :: Arrow a => v -> a (b,v) c -> a b c
inject v f = proc b -> f -< (b,v)

g :: ArrowPlus a => a (Int,Int) String
g = proc (x,y) ->
	(|inject "hello"|) (
		(|handle|)
			(\msg -> returnA -< msg ++ show y)
			(\s msg -> returnA -< s ++ show x)
	)

h :: ArrowPlus a => a (Int,Int) String
h = proc (x,y) ->
	(|inject "hello"|) (
		(\msg -> returnA -< msg ++ show y)
		<+>
		(\msg -> returnA -< msg ++ show x)
	)
