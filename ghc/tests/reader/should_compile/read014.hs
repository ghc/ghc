-- !!! Empty export lists are legal (and useful.)
module T () where

ng1 x y = negate y

instance (Num a, Num b) => Num (a,b)
  where
   negate (a,b) = (ng 'c' a, ng1 'c' b)   where  ng x y = negate y
