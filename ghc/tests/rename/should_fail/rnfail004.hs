-- !!! multiple definitions, but hidden in patterns
module Foo where

f x = x
  where
    a		= []
    (b,c,a)	= ([],[],d)
    [d,b,_]	= ([],a,[])
