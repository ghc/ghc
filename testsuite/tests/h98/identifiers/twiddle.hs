module Twiddle where

main = let foo@(~(x,y)) = (1,2)
       in print foo
