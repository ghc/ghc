module ShouldFail where

-- !!! Test for parse error in do/let expression

foo = do let foo = True
	     return () 
           

-- Note the let binding at the end!
-- This gave a pattern-match failure in tcStmts in ghc-4.04proto

h x = x



