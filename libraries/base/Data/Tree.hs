-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Multi-way trees (/aka/ rose trees) and forests.
--
-- Also included are neat presentations for trees and forests.
--
-----------------------------------------------------------------------------

module Data.Tree(
	Tree(..), Forest,
	flatten, levels,
    ) where

-- | Multi-way trees, also known as /rose trees/.
data Tree a   = Node a (Forest a) -- ^ a value and zero or more child trees.
type Forest a = [Tree a]

instance Functor Tree where
  fmap = mapTree

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

-- explicit instance for Haddock's benefit
instance Eq a => Eq (Tree a) where
  Node x ts == Node x' ts'  =  x == x' && ts == ts'

instance Show a => Show (Tree a) where
  show = showTree
  showList ts s = showForest ts ++ s

showTree :: Show a => Tree a -> String
showTree  = drawTree . mapTree show

showForest :: Show a => Forest a -> String
showForest  = unlines . map showTree

drawTree :: Tree String -> String
drawTree  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = grp this (space (length this)) (stLoop ts0)
 where this          = s1 ++ x ++ " "

       space n       = replicate n ' '

       stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop []     = error "rsLoop:Unexpected empty list."
       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst0 rst  = zipWith (++) (fst0:repeat rst)

       [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
 where squish (Node x ts) xs = x:foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t = map (map root) $ takeWhile (not . null) $ iterate subforest [t]
 where root (Node x _) = x
       subforest f     = [t | Node _ ts <- f, t <- ts]
