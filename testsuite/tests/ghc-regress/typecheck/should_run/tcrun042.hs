{-# LANGUAGE TupleSections, RankNTypes, ImpredicativeTypes #-}
module Main where

e :: a -> (forall b. b -> b -> b) -> (a, String, forall c. c -> c -> c)
e = (,"Hello" ++ "World",)

dropFunction :: (a, String, forall c. c -> c -> c) -> (a, String, Int)
dropFunction (x, y, z) = (x, y, z 10 20)

main = print (dropFunction $ e "Meh" (flip const), dropFunction $ e 10 const)