{-# OPTIONS -fglasgow-exts #-}

-- A type signature on the LHS of a do-stmt was a parse
-- error in 6.4.2, but ok thereafter

module ShouldCompile where

f () = do { x :: Bool <- return True 
	  ; return x }
