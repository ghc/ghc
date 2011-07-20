module Main where

data Y = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8
	deriving( Show )

data X = WithY Y
         | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8

foo :: X -> Y
foo A1 = X1
foo A2 = X2
foo A3 = X3
foo A4 = X4
foo A5 = X5
foo A6 = X6
foo A7 = X7
foo A8 = X8
foo (WithY _) = X1

bar :: X -> Y
bar (WithY x) = x
bar y = foobar (foo y)	-- The WithY case can't occur, and in an 
			-- earlier version that confused the code generator

{-# NOINLINE foobar #-}
foobar x = x


main = print (map bar [WithY X2, A4, A5])
