-- Test Trac #4042

module T4042 where

f :: A -> A
f
-- The above line is a naked Template Haskell splice
-- When compiling without -XTemplateHaskell we don't
-- want a confusing error messsage saying "A is not in scope"

data A = A

