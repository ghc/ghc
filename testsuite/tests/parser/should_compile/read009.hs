-- !!! combining undeclared infix operators
module ShouldCompile where

-- should default to 'infixl 9'

test = let f x y = x+y in 1 `f` 2 `f` 3

