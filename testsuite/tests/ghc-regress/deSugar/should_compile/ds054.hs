module ShouldCompile where

-- !!! should produce a warning about an unused identifer
x :: [()]
x = [ () | y <- [] ]
