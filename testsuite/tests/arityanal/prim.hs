module Prim where

map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

zipWith2 f [] [] = []
zipWith2 f (a:x) (b:y) = (f a b):zipWith2 f x y