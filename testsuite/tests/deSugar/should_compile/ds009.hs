-- !!! ds009 -- simple list comprehensions

module ShouldCompile where

f xs = [ x | x <- xs ]

g xs ys zs = [ (x,y,z) | x <- xs, y <- ys, z <- zs, True ]

h xs ys = [ [x,y] | x <- xs, y <- ys, False ]

i xs = [ x | all@(x,y) <- xs, all == ([],[]) ]

j xs = [ (a,b) | (a,b,c,d) <- xs ]
