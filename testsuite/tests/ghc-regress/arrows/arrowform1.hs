{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

import Control.Arrow

handle :: ArrowPlus a => a b c -> a (b,String) c -> a b c
handle b h = b <+> (arr (\b -> (b,"")) >>> h)

f :: ArrowPlus a => a (Int,Int) String
f = proc (x,y) -> (|handle|) (returnA -< show y) (\s -> returnA -< s ++ show x)

inject :: Arrow a => v -> a (b,v) c -> a b c
inject v f = arr (\b -> (b,v)) >>> f
-- inject v f = proc b -> f -< (b,v)

g :: ArrowPlus a => a (Int,Int) String
g = proc (x,y) ->
	(|inject "hello"|)
		((|handle|)
			(\msg -> returnA -< msg ++ show y)
			(\s msg -> returnA -< s ++ show x))
