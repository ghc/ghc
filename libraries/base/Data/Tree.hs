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
	-- * Building trees
	unfoldTree, unfoldForest,
	unfoldTreeM, unfoldForestM,
	unfoldTreeM_BF, unfoldForestM_BF,
    ) where

#ifdef __HADDOCK__
import Prelude
#endif

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad
import Data.Monoid (Monoid(..))
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
			ViewL(..), ViewR(..), viewl, viewr)
import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Data.Typeable

#ifdef __GLASGOW_HASKELL__
import Data.Generics.Basics (Data)
#endif

-- | Multi-way trees, also known as /rose trees/.
data Tree a   = Node {
		rootLabel :: a,		-- ^ label value
		subForest :: Forest a	-- ^ zero or more child trees
	}
#ifndef __HADDOCK__
# ifdef __GLASGOW_HASKELL__
  deriving (Eq, Read, Show, Data)
# else
  deriving (Eq, Read, Show)
# endif
#else /* __HADDOCK__ (which can't figure these out by itself) */
instance Eq a => Eq (Tree a)
instance Read a => Read (Tree a)
instance Show a => Show (Tree a)
instance Data a => Data (Tree a)
#endif
type Forest a = [Tree a]

#include "Typeable.h"
INSTANCE_TYPEABLE1(Tree,treeTc,"Tree")

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (map (fmap f) ts)

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

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
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t = map (map rootLabel) $
		takeWhile (not . null) $
		iterate (concatMap subForest) [t]

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
	(a, bs) <- f b
	ts <- unfoldForestM f bs
	return (Node a ts)

-- | Monadic forest builder, in depth-first order
#ifndef __NHC__
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
#endif
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where getElement xs = case viewl xs of
		x :< _ -> x
		EmptyL -> error "unfoldTreeM_BF"

-- | Monadic forest builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

-- takes a sequence (queue) of seeds
-- produces a sequence (reversed queue) of trees of the same length
unfoldForestQ :: Monad m => (b -> m (a, [b])) -> Seq b -> m (Seq (Tree a))
unfoldForestQ f aQ = case viewl aQ of
	EmptyL -> return empty
	a :< aQ -> do
		(b, as) <- f a
		tQ <- unfoldForestQ f (Prelude.foldl (|>) aQ as)
		let (tQ', ts) = splitOnto [] as tQ
		return (Node b ts <| tQ')
  where splitOnto :: [a'] -> [b'] -> Seq a' -> (Seq a', [a'])
	splitOnto as [] q = (q, as)
	splitOnto as (_:bs) q = case viewr q of
		q' :> a -> splitOnto (a:as) bs q'
		EmptyR -> error "unfoldForestQ"
