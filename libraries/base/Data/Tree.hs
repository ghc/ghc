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
-----------------------------------------------------------------------------

module Data.Tree(
	Tree(..), Forest,
	-- * Two-dimensional drawing
	drawTree, drawForest,
	-- * Extraction
	flatten, levels,
    ) where

#ifdef __HADDOCK__
import Prelude
#endif

-- | Multi-way trees, also known as /rose trees/.
data Tree a   = Node a (Forest a) -- ^ a value and zero or more child trees.
#ifndef __HADDOCK__
  deriving (Eq, Read, Show)
#else /* __HADDOCK__ (which can't figure these out by itself) */
instance Eq a => Eq (Tree a)
instance Read a => Read (Tree a)
instance Show a => Show (Tree a)
#endif
type Forest a = [Tree a]

instance Functor Tree where
  fmap = mapTree

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t = map (map root) $ takeWhile (not . null) $ iterate subforest [t]
  where root (Node x _) = x
	subforest f     = [t | Node _ ts <- f, t <- ts]
