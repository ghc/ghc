-- !!! ds028: failable pats in top row

module ShouldCompile where


-- when the first row of pats doesn't have convenient
-- variables to grab...

mAp f []	= []
mAp f (x:xs)	= f x : mAp f xs

True  |||| _	=  True
False |||| x	=  x
