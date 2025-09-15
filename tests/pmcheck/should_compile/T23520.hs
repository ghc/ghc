module T23520 where

data T = T1 { x :: Bool } | T2

f a = a { x = False }
