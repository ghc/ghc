{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
--
-- See also
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
--
-----------------------------------------------------------------------------

module Data.Traversable (
    -- * The 'Traversable' class
    Traversable(..),
    -- * Utility functions
    for,
    forM,
    mapAccumL,
    mapAccumR,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,
    ) where

-- It is convenient to use 'Const' here but this means we must
-- define a few instances here which really belong in Control.Applicative
import Control.Applicative ( Const(..), ZipList(..) )
import Data.Either ( Either(..) )
import Data.Foldable ( Foldable )
import Data.Functor
import Data.Monoid ( Dual(..), Sum(..), Product(..), First(..), Last(..) )
import Data.Proxy ( Proxy(..) )

import GHC.Arr
import GHC.Base ( Applicative(..), Monad(..), Monoid, Maybe(..),
                  ($), (.), id, flip )
import GHC.Generics
import qualified GHC.List as List ( foldr )

-- | Functors representing data structures that can be traversed from
-- left to right.
--
-- A definition of 'traverse' must satisfy the following laws:
--
-- [/naturality/]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'traverse' Identity = Identity@
--
-- [/composition/]
--   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
--
-- A definition of 'sequenceA' must satisfy the following laws:
--
-- [/naturality/]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'sequenceA' . 'fmap' Identity = Identity@
--
-- [/composition/]
--   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
--
-- where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
--  * @t ('pure' x) = 'pure' x@
--
--  * @t (x '<*>' y) = t x '<*>' t y@
--
-- and the identity functor @Identity@ and composition of functors @Compose@
-- are defined as
--
-- >   newtype Identity a = Identity a
-- >
-- >   instance Functor Identity where
-- >     fmap f (Identity x) = Identity (f x)
-- >
-- >   instance Applicative Identity where
-- >     pure x = Identity x
-- >     Identity f <*> Identity x = Identity (f x)
-- >
-- >   newtype Compose f g a = Compose (f (g a))
-- >
-- >   instance (Functor f, Functor g) => Functor (Compose f g) where
-- >     fmap f (Compose x) = Compose (fmap (fmap f) x)
-- >
-- >   instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- >     pure x = Compose (pure (pure x))
-- >     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
--
-- (The naturality law is implied by parametricity.)
--
-- Instances are similar to 'Functor', e.g. given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Traversable Tree where
-- >    traverse f Empty = pure Empty
-- >    traverse f (Leaf x) = Leaf <$> f x
-- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
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
    {-# MINIMAL traverse | sequenceA #-}

    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right, and
    -- and collect the results. For a version that ignores the results
    -- see 'Data.Foldable.sequenceA_'.
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

    -- | Map each element of a structure to a monadic action, evaluate
    -- these actions from left to right, and collect the results. For
    -- a version that ignores the results see 'Data.Foldable.mapM_'.
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = traverse

    -- | Evaluate each monadic action in the structure from left to
    -- right, and collect the results. For a version that ignores the
    -- results see 'Data.Foldable.sequence_'.
    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA

-- instances for Prelude types

instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = (:) <$> f x <*> ys

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

instance Ix i => Traversable (Array i) where
    traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = pure Proxy
    {-# INLINE mapM #-}
    sequence _ = pure Proxy
    {-# INLINE sequence #-}

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

instance Traversable Dual where
    traverse f (Dual x) = Dual <$> f x

instance Traversable Sum where
    traverse f (Sum x) = Sum <$> f x

instance Traversable Product where
    traverse f (Product x) = Product <$> f x

instance Traversable First where
    traverse f (First x) = First <$> traverse f x

instance Traversable Last where
    traverse f (Last x) = Last <$> traverse f x

instance Traversable ZipList where
    traverse f (ZipList x) = ZipList <$> traverse f x

-- Instances for GHC.Generics
instance Traversable U1 where
    traverse _ _ = pure U1
    {-# INLINE traverse #-}
    sequenceA _ = pure U1
    {-# INLINE sequenceA #-}
    mapM _ _ = pure U1
    {-# INLINE mapM #-}
    sequence _ = pure U1
    {-# INLINE sequence #-}

deriving instance Traversable V1
deriving instance Traversable Par1
deriving instance Traversable f => Traversable (Rec1 f)
deriving instance Traversable (K1 i c)
deriving instance Traversable f => Traversable (M1 i c f)
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)
deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)
deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)
deriving instance Traversable UAddr
deriving instance Traversable UChar
deriving instance Traversable UDouble
deriving instance Traversable UFloat
deriving instance Traversable UInt
deriving instance Traversable UWord

-- general functions

-- | 'for' is 'traverse' with its arguments flipped. For a version
-- that ignores the results see 'Data.Foldable.for_'.
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE for #-}
for = flip traverse

-- | 'forM' is 'mapM' with its arguments flipped. For a version that
-- ignores the results see 'Data.Foldable.forM_'.
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
{-# INLINE forM #-}
forM = flip mapM

-- left-to-right state transformer
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateL s) where
    pure x = StateL (\ s -> (s, x))
    StateL kf <*> StateL kv = StateL $ \ s ->
        let (s', f) = kf s
            (s'', v) = kv s'
        in (s'', f v)

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s

-- right-to-left state transformer
newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
    fmap f (StateR k) = StateR $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateR s) where
    pure x = StateR (\ s -> (s, x))
    StateR kf <*> StateR kv = StateR $ \ s ->
        let (s', v) = kv s
            (s'', f) = kf s'
        in (s'', f v)

-- |The 'mapAccumR' function behaves like a combination of 'fmap'
-- and 'foldr'; it applies a function to each element of a structure,
-- passing an accumulating parameter from right to left, and returning
-- a final value of this accumulator together with the new structure.
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'traverse' is defined. (Using
--   `fmapDefault` with a `Traversable` instance defined only by
--   'sequenceA' will result in infinite recursion.)
fmapDefault :: Traversable t => (a -> b) -> t a -> t b
{-# INLINE fmapDefault #-}
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

