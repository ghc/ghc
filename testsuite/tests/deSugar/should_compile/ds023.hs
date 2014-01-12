-- !!! ds023 -- overloading eg from section 9.2
--
module ShouldCompile where

f x	= g (x == x) x
g b x	= abs (f x)
--g b x	= (f x) + (f x)
