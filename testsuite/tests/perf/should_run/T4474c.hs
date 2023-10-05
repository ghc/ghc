{-# LANGUAGE BangPatterns #-}

module Main where

data Tree = Leaf !Int | Fork !Tree !Tree deriving Show

fullTree 0 = Leaf 1
fullTree n = let t = fullTree (n - 1) in Fork t t

flatListNaive (Leaf n)   = [n]
flatListNaive (Fork a b) = flatListNaive a ++ flatListNaive b

flatListCons t = flat t []
  where
  flat (Leaf n)   ns = n : ns
  flat (Fork a b) ns = flat a (flat b ns)

flatListCons2 t = flat t []
  where
  flat (Leaf n)   = \ns -> n : ns
  flat (Fork a b) = \ns -> flat a (flat b ns)

flatListCons3 t = flat t []
  where
  flat (Leaf n)   = (n :)
  flat (Fork a b) = flat a . flat b

flatDList (Leaf n)   = (n :)
flatDList (Fork a b) = flatDList a . flatDList b

sumList l = loop 0 l
  where loop !c [] = c
        loop !c (h:t) = loop (c + h) t

sumDList l = loop 0 (l [])
  where loop !c []      = c
        loop !c (h : t) = loop (c + h) t

main = print $ sumList  $ flatListCons3 $ fullTree 26

