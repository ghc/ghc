-- !!! ds003 -- list, tuple, lazy, as patterns
--
module ShouldCompile where

f []		y	 True  = []
f x		a@(y,ys) ~z    = []
f (x:x1:x2:x3)	~(y,ys)  z     = []
f x		y        True  = []
