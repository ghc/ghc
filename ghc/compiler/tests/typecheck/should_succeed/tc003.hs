module ShouldSucceed where

-- This is a somewhat surprising program.
-- It shows up the monomorphism restriction, *and* ambiguity resolution!
-- The binding is a pattern binding without a signature, so it is monomorphic.
-- Hence the types of c,d,e are not universally quantified.  But then
-- their type variables are ambiguous, so the ambiguity resolution leaps
-- into action, and resolves them to Integer.

-- That's why we check the interface file in the test suite.

(c@(d,e)) = if True then (1,2) else (1,3)
