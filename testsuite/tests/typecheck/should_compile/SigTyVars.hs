{-# LANGUAGE ScopedTypeVariables #-}

module SigTyVars where

-- Here the lexically scoped type variables 'a' and 'b'
-- both map to the same skolem 'x'.  It's perhaps a bit
-- surprising, but it's awkward to prevent, and it seems
-- easier to leave it.

f :: x -> x -> [x]
f (x::a) (y::b) = [x::b, y::a]

