{-# LANGUAGE BangPatterns #-}
module Micro.Load (mkBigTree, mkBigTrees) where

import Micro.Types
import Micro.ReadShow ()


mkBigTrees :: Int -> Int -> [Tree]
mkBigTrees n depth =
   let !tree = mkBigTree depth
    in replicate n tree

mkBigTree :: Int -> Tree
mkBigTree 0     = Leaf
mkBigTree depth =
  let !subtree = mkBigTree (depth-1)
   in Fork subtree subtree

