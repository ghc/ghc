{- 

From: Olaf Chitil <chitil@Informatik.RWTH-Aachen.DE>

It is a problem with 0.29 (which we use for compiling 2.01), it is gone
in 2.01.

	f :: Eq a => a -> [b] -> [b] -> Bool
	f a [] [] = (a==a)
	main = print (f True "" "Hallo")


when run after compilation with 0.29 you get:
Fail: "test.hs", line 6: incomplete pattern(s) to match in function "ds.d5b4"

while 2.01 gives you as desired
Fail: In pattern-matching: function f{-aYw-}; at test.hs, line 6

The problem is the dictionary, because for the program

	f :: a -> [b] -> [b] -> Bool
	f a [] [] = True
	main = print (f True "" "Hallo")

0.29 gives the function name "f" as well.

So it's ok in 2.01, but why did you change the form of the error messages?
"incomplete pattern(s) to match" is more informative then "In pattern-matching"!
I even prefer the order of information in the 0.29 error messages.

May I finally repeat that in my opinion the compiler should warn about
incomplete patterns during compilation. However, I suppose the
incomplete patterns are just recognised by the desugarer which does
not produce error messages any more.

-}


module Main where

f :: Eq a => a -> [b] -> [b] -> Bool
f a [] [] = (a==a)

main = print (f True "" "Hallo")


