module Foo where

--!!! multiple definitions, but hidden in patterns

f x = x
  where
    a		= []
    (b,c,a)	= ([],[],d)
    [d,b,_]	= ([],a,[])
