module ShouldFail where

-- Erroneously allowed by GHC 6.2.x
f x = case x of
	 False -> do 
    { return x; }
-- this line should close the 'case' context and cause the 'do' to be empty.
