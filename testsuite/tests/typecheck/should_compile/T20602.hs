{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module T20602 where

f :: (Eq a, Ord a) => a -> Bool
f x = x == x

g :: (Eq a, Ord a) => a -> Bool
g x = x > x

h :: (Eq a, Ord a) => a -> Bool
h x = x == x && x > x

j :: (Eq a, a ~ b, Eq b) => a -> Bool
j x = x == x

k :: (Eq a, a ~ b, Eq b) => a -> Bool
k _ = True

l :: (Eq a, Ord a) => a -> Bool
l _ = True

m :: (a ~ b) => a -> a
m x = x

n :: (Eq a, b ~ a) => a -> Bool
n x = (==) x x
