module ShouldFail where

-- !!! Do-notation requires an *expression* at the end.

foo = do let foo = True
	     return () 
           

-- Note the let binding at the end!
-- This gave a pattern-match failure in tcStmts in ghc-4.04proto

h x = x



