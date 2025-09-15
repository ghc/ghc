{-# LANGUAGE GADTs #-}

module T9939 where

f1 :: (Eq a, Ord a) => a -> a -> Bool
-- Eq a redundant
f1 x y = (x == y) && (x > y)

f2 :: (Eq a, Ord a) => a -> a -> Bool
-- Ord a redundant, but Eq a is reported
f2 x y = (x == y)

f3 :: (Eq a, a ~ b, Eq b) => a -> b -> Bool
-- Eq b redundant
f3 x y = x==y

data Equal a b where
  EQUAL :: Equal a a

f4 :: (Eq a, Eq b) => a -> b -> Equal a b -> Bool
-- Eq b redundant
f4 x y EQUAL = y==y
