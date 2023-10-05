-- Made the new demand analyser enter an absent arg 
-- Reason: it thought 'a' was unused in g.

module Main where

-- Strictness: SS(AL) -> T
f True p@(x,y) = (p,y)
f False p@(x,y) = f y p

-- Easy to get the wrong strictness,
-- by thinking 'a' is absent
g True  a b = f False (a,b)
g False a b = g b a b

main = print (g True 'a' True)
