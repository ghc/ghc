-- !!! ds002 -- overlapping equations and guards
--
-- this tests "overlapping" variables and guards

module ShouldCompile where

f x = x
f y = y
f z = z

g x y z | True = f z
    	| True = f z
    	| True = f z
g x y z | True = f z
    	| True = f z
    	| True = f z
