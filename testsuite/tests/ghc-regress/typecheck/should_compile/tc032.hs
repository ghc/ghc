module ShouldSucceed where

data AList b = Node b [b] | Other (b,Char)
