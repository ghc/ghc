{-# LANGUAGE Generics #-}

module Main where

import GHC.Generics
import Uniplate


data Tree     = Leaf | Node Int Tree Tree deriving Show
data Pair a b = Pair a b                  deriving Show

instance Uniplate Tree
instance Uniplate (Pair a b)

-- Tests
t1 = children ('p')
t2 = children (Pair "abc" (Pair "abc" 2))
t3 = children (Node 2 Leaf Leaf)

main = print (t1, t2, t3)
