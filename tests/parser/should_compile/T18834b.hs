{-# OPTIONS -Woperator-whitespace #-}

module T18834b where

f a b = a+ b
g a b = a +b
h a b = a+b
k a b = a + b  -- this one is OK, no warning
