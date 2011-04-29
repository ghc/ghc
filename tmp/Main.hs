{-# LANGUAGE FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveRepresentable #-}
{-# LANGUAGE Generics #-}

module Main where

import GHC.Generics

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Representable0

instance Representable0 Char
instance (Representable0 (Tree a)) => Show (Tree a)

-- deriving instance Representable0 (Tree a)

tree1, tree2 :: Tree Char
tree1 = Node 'a' Leaf  Leaf
tree2 = Node 'c' tree1 tree1

main = print tree2
