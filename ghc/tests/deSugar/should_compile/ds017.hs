--!!! ds017 -- let expressions
--
module Tests where

f x y z
  = let
	a = x : []
	b = x : a
	c = y (let d = (z, z) in d)
	result = (c, b)
    in
	result
