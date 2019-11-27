module Proposal229b ((~), (@)) where

(~) :: a -> b -> (a, b)
x ~ y = (x, y)

(@) :: a -> b -> (a, b)
x @ y = (x, y)

r :: ((Bool, Bool), Bool)
r = True ~ False @ True
