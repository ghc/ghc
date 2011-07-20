-- !!! ds021 -- hairier uses of guards

module ShouldCompile where

f x y z | x == y     = []
	| x /= z     = []
	| True	     = []
	| False	     = []
