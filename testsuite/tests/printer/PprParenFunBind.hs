module ParenFunBind where

(foo x)   y  = x + y
((bar x)) y  = x + y
((baz x)) (y) = x + y
