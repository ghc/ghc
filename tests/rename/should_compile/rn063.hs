
{-# OPTIONS_GHC -Wall #-}

module Foo where

-- We should complain that both x and y are unused.
-- We used to not warn for recursive bindings, like x (trac #2136).

v :: a
v = let x = x in undefined

w :: a
w = let y = 'a' in undefined

