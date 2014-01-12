-- Uses of built-in syntax should provoke a decent error message

module ShouldFail where

data T0 = ()
data T3= (,,,)
data Nil = []
data List = Int : Bool 

