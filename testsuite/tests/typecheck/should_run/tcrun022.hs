-- This test checks in which way the type checker handles phantom types in
-- RULES.  We would like these type variables to be generalised, but some
-- versions of GHC instantiated them to `()', which seriously limited the
-- applicability of such RULES.

module Main (main)
where

data T a = C

foo :: T a -> String
{-# NOINLINE foo #-}
foo C = "rewrite rule did NOT fire"

{-# RULES 

-- this rule will not fire if the type argument of `T' is constrained to `()'
--
"foo/C" foo C = "rewrite rule did fire"

 #-}

main = putStrLn $ foo (C :: T Int)
