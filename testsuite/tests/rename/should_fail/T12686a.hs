module T12686a where

x = True

data Bad = MkBad 'x
-- The 'x should be rejected in a civilised way