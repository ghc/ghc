-- !!! Parallel list comprehensions w/ parallel lets

module ShouldCompile where

xys = [ (x, y) | let a = 13, x <- [a .. 19] | let a = 17, y <- [a .. 23] ]
