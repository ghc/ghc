-- This test checks that constructors with strictness annotations
-- at least parse correctly.  In GHC 2.02 they didn't!

module Main where
 
data Foo1 = Crunch1 ! Int ! Int Int deriving( Show )

data Foo2 = Crunch2 ! Int Int Int   deriving( Show )

main = do
	print (Crunch1 (1+1) (2+2) (3+3))
	print (Crunch2 (1+1) (2+2) (3+3))
  
