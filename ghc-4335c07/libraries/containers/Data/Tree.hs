{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
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
    flatten, levels, foldTree,
    -- * Building trees
    unfoldTree, unfoldForest,
    unfoldTreeM, unfoldForestM,
    unfoldTreeM_BF, unfoldForestM_BF,
    ) where

#if MIN_VERSION_base(4,8,0)
import Data.Foldable (toList)
import Control.Applicative (Applicative(..), liftA2)
#else
import Control.Applicative (Applicative(..), liftA2, (<$>))
import Data.Foldable (Foldable(foldMap), toList)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
#endif

import Control.Monad (liftM)
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Data.Typeable
import Control.DeepSeq (NFData(rnf))

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif

#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip (..))
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Coerce
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
import Data.Semigroup (Semigroup (..))
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
#endif

-- | Multi-way trees, also known as /rose trees/.
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 706
  deriving (Eq, Read, Show, Data, Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 702
  deriving (Eq, Read, Show, Data, Generic)
#else
  deriving (Eq, Read, Show, Data)
#endif
#else
  deriving (Eq, Read, Show)
#endif
type Forest a = [Tree a]

#if MIN_VERSION_base(4,9,0)
instance Eq1 Tree where
  liftEq eq = leq
    where
      leq (Node a fr) (Node a' fr') = eq a a' && liftEq leq fr fr'

instance Ord1 Tree where
  liftCompare cmp = lcomp
    where
      lcomp (Node a fr) (Node a' fr') = cmp a a' <> liftCompare lcomp fr fr'

instance Show1 Tree where
  liftShowsPrec shw shwl p (Node a fr) = showParen (p > 10) $
        showString "Node {rootLabel = " . shw 0 a . showString ", " .
          showString "subForest = " . liftShowList shw shwl fr .
          showString "}"

instance Read1 Tree where
  liftReadsPrec rd rdl p = readParen (p > 10) $
    \s -> do
      ("Node", s1) <- lex s
      ("{", s2) <- lex s1
      ("rootLabel", s3) <- lex s2
      ("=", s4) <- lex s3
      (a, s5) <- rd 0 s4
      (",", s6) <- lex s5
      ("subForest", s7) <- lex s6
      ("=", s8) <- lex s7
      (fr, s9) <- liftReadList rd rdl s8
      ("}", s10) <- lex s9
      pure (Node a fr, s10)
#endif

INSTANCE_TYPEABLE1(Tree)

instance Functor Tree where
    fmap = fmapTree
    x <$ Node _ ts = Node x (map (x <$) ts)

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)
#if MIN_VERSION_base(4,8,0)
-- Safe coercions were introduced in 4.7.0, but I am not sure if they played
-- well enough with RULES to do what we want.
{-# NOINLINE [1] fmapTree #-}
{-# RULES
"fmapTree/coerce" fmapTree coerce = coerce
 #-}
#endif

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)
#if MIN_VERSION_base(4,10,0)
    liftA2 f (Node x txs) ty@(Node y tys) =
        Node (f x y) (map (f x <$>) tys ++ map (\tx -> liftA2 f tx ty) txs)
#endif
    Node x txs <* ty@(Node _ tys) =
        Node x (map (x <$) tys ++ map (<* ty) txs)
    Node _ txs *> ty@(Node y tys) =
        Node y (tys ++ map (*> ty) txs)

instance Monad Tree where
    return = pure
    Node x ts >>= f = Node x' (ts' ++ map (>>= f) ts)
      where Node x' ts' = f x

instance Traversable Tree where
    traverse f (Node x ts) = liftA2 Node (f x) (traverse (traverse f) ts)

instance Foldable Tree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

#if MIN_VERSION_base(4,8,0)
    null _ = False
    {-# INLINE null #-}
    toList = flatten
    {-# INLINE toList #-}
#endif

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

#if MIN_VERSION_base(4,4,0)
instance MonadZip Tree where
  mzipWith f (Node a as) (Node b bs)
    = Node (f a b) (mzipWith (mzipWith f) as bs)

  munzip (Node (a, b) ts) = (Node a as, Node b bs)
    where (as, bs) = munzip (map munzip ts)
#endif

-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
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
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

-- | Catamorphism on trees.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

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
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
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
    a :< aQ' -> do
        (b, as) <- f a
        tQ <- unfoldForestQ f (Prelude.foldl (|>) aQ' as)
        let (tQ', ts) = splitOnto [] as tQ
        return (Node b ts <| tQ')
  where
    splitOnto :: [a'] -> [b'] -> Seq a' -> (Seq a', [a'])
    splitOnto as [] q = (q, as)
    splitOnto as (_:bs) q = case viewr q of
        q' :> a -> splitOnto (a:as) bs q'
        EmptyR -> error "unfoldForestQ"
