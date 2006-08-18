{-# OPTIONS -fglasgow-exts #-}

-- Scoped type variables on pattern bindings
-- This should *fail* on GHC 5.02 and lower, 
-- It's a post-5.02 enhancements to allow them.

-- It's an error again in GHC 6.6!

module ShouldCompile where

f x = let (p::a,q::a) = x in (q::a,p)

g a b = let y::a = a in 
        let  v :: a
	     v = b
	 in v
	