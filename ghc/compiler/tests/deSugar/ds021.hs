--!!! ds021 -- hairier uses of guards

module Test where

f x y z | x == y     = []
	| x /= z     = []
	| True	     = []
	| False	     = []
