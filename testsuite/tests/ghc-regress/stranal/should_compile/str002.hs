-- !!! Recursive newtypes
-- 	Needs -O
-- This one made GHC < 5.00.2 go into an 
-- infinite loop in the strictness analysier

module Foo where

newtype V = MkV V

f :: V -> V
f (MkV v) = v

