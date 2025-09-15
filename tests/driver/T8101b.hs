
module A where

data ABC = A | B | C

abc :: ABC -> Int
abc x = case x of
    A -> 1
