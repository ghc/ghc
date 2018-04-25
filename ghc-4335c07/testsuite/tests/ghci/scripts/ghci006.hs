
module Ghci006 where

data Q = forall x . Show x => Q x
showQ (Q x) = show x

-- associated bug is that at the interpreter command line,
--   showQ (Q "foo") crashed the interpreter.
