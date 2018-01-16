module ShouldCompile where

-- !!! Section precedences

-- infixl 6  +, -
-- infixr 5  ++, :

f = (++ [] ++ [])
g = (3 + 4 +)

-- prefix negation is like infixl 6.
h x = (-x -)
