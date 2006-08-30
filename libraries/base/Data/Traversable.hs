-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
--
-- See also
--
--  * /Applicative Programming with Effects/,
--    by Conor McBride and Ross Paterson, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * /The Essence of the Iterator Pattern/,
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, and online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
-- Note that the functions 'mapM' and 'sequence' generalize "Prelude"
-- functions of the same names from lists to any 'Traversable' functor.
-- To avoid ambiguity, either import the "Prelude" hiding these names
-- or qualify uses of these function names with an alias for this module.

module Data.Traversable (
	Traversable(..),
	for,
	forM,
	fmapDefault,
	foldMapDefault,
	) where

import Prelude hiding (mapM, sequence, foldr)
import qualified Prelude (mapM, foldr)
import Control.Applicative
import Data.Foldable (Foldable())
import Data.Monoid (Monoid)
import Data.Array

-- | Functors representing data structures that can be traversed from
-- left to right.
--
-- Minimal complete definition: 'traverse' or 'sequenceA'.
--
-- Instances are similar to 'Functor', e.g. given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Traversable Tree
-- >	traverse f Empty = pure Empty
-- >	traverse f (Leaf x) = Leaf <$> f x
-- >	traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- This is suitable even for abstract types, as the laws for '<*>'
-- imply a form of associativity.
--
-- The superclass instances should satisfy the following:
--
--  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
--    with the identity applicative functor ('fmapDefault').
--
--  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
--    equivalent to traversal with a constant applicative functor
--    ('foldMapDefault').
--
class (Functor t, Foldable t) => Traversable t where
	-- | Map each element of a structure to an action, evaluate
	-- these actions from left to right, and collect the results.
	traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
	traverse f = sequenceA . fmap f

	-- | Evaluate each action in the structure from left to right,
	-- and collect the results.
	sequenceA :: Applicative f => t (f a) -> f (t a)
	sequenceA = traverse id

	-- | Map each element of a structure to an monadic action, evaluate
	-- these actions from left to right, and collect the results.
	mapM :: Monad m => (a -> m b) -> t a -> m (t b)
	mapM f = unwrapMonad . traverse (WrapMonad . f)

	-- | Evaluate each monadic action in the structure from left to right,
	-- and collect the results.
	sequence :: Monad m => t (m a) -> m (t a)
	sequence = mapM id

-- instances for Prelude types

instance Traversable Maybe where
	traverse f Nothing = pure Nothing
	traverse f (Just x) = Just <$> f x

instance Traversable [] where
	traverse f = Prelude.foldr cons_f (pure [])
	  where cons_f x ys = (:) <$> f x <*> ys

	mapM = Prelude.mapM

instance Ix i => Traversable (Array i) where
	traverse f arr = listArray (bounds arr) <$> traverse f (elems arr)

-- general functions

-- | 'for' is 'traverse' with its arguments flipped.
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE for #-}
for = flip traverse

-- | 'forM' is 'mapM' with its arguments flipped.
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
{-# INLINE forM #-}
forM = flip mapM

-- | This function may be used as a value for `fmap` in a `Functor` instance.
fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f = getId . traverse (Id . f)

-- | This function may be used as a value for `Data.Foldable.foldMap`
-- in a `Foldable` instance.
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)

-- local instances

newtype Id a = Id { getId :: a }

instance Functor Id where
	fmap f (Id x) = Id (f x)

instance Applicative Id where
	pure = Id
	Id f <*> Id x = Id (f x)
