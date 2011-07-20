{- 
Date: Tue, 20 May 1997 05:10:04 GMT
From: Tomasz Cholewo <tjchol01@mecca.spd.louisville.edu>

ghc-2.03 cannot compile the following code, which I think is correct
according to the Report

	data X = A {a :: Int} | B {a :: Int}

The error message is: 

    Conflicting definitions for:  a
	Defined at bug4.lhs:2
	Defined at bug4.lhs:2

In addition the following snippet

	data X = A {a :: Int}
	y = let A {a} = x
	    in a

fails with:

    bug4.lhs:4:5: Not a valid LHS on input: "in"
-}
--module Main(main) where

data X = A {a :: Int} | B {a :: Int}

f x = let A {a=a} = x
      in a

main = print (f (A {a = 3}))
