module MutRec where

-- Mutual recursion with different
-- names for the same type variable
f t = x
  where
    x :: [a]
    y :: b
    (x,y,z,r) = ([y,z], z, head x, t)


