
-- !!! Test monomorphism + RULES

module ShouldCompile where

-- This example crashed GHC 4.08.1.
-- The reason was that foobar is monomorphic, so the RULE 
-- should not generalise over it.

{-# NOINLINE [1] foo #-}
foo 1 = 2
bar 0 = 1

foobar = 2

{-# RULES
  "foo/bar" foo bar = foobar
 #-}


