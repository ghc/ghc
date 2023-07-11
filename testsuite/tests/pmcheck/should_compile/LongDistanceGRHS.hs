module LongDistanceGRHS where

data D = T1 | T2

foo1 y
  | x <- y
  = case x of
       T1 -> case y of { T1 -> False }
       T2 -> True

foo2 y
  | x <- y
  = case y of
       T1 -> case x of { T1 -> False }
       T2 -> True

foo3 y
  | x@T1 <- y
  = case y of
       T1 -> False
  | otherwise
  = True
