{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- This tests the recursive-dictionary stuff.
--
-- The derived instance needs 

module Main where

data Fix f = In (f (Fix f))  deriving( Show, Eq )
data L x = Nil | Cons Int x  deriving (Show, Eq)

main = do { print (In Nil); 
	    print (In Nil == In Nil) }

