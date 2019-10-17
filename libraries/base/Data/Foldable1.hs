{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable1
-- Copyright   :  Edward Kmett
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be folded to a summary value.
--
-- @since 4.14.0.0
--
-----------------------------------------------------------------------------

module Data.Foldable1 (
    Foldable1(..),
    intercalate1,
    foldrM1,
    foldlM1,
    maximum1By,
    minimum1By,
    ) where

import Data.Functor.Utils ((#.))
import Data.Semigroup
import Data.Foldable (Foldable, foldlM, foldr)
import qualified Data.Monoid as Mon
import GHC.List (foldl, foldl')

import GHC.Base hiding (foldr)
import GHC.Generics

import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity (..))

import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum     as Functor

import qualified Data.List.NonEmpty as NE

-- TODO: Data.Functor.Compose
-- TODO: Data.Functor.Product
-- TODO: Data.Functor.Identity
-- TODO: Data.Functor.Sum

-- | Non-empty data structures that can be folded.
--
-- @since 4.14.0.0
class Foldable t => Foldable1 t where
    {-# MINIMAL foldMap1 | toNonEmpty | foldr1map #-}

    -- foldMap1 defined using foldr1map
    -- foldr1map defined using toNonEmpty
    -- toNonEmpty defined using foldMap1

    -- | Combine the elements of a structure using a semigroup.
    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    -- | Map each element of the structure to a semigroup,
    -- and combine the results.
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m
    foldMap1 f = foldr1map f (<>)

    -- | A variant of 'foldMap1' that is strict in the accumulator.
    foldMap1' :: Semigroup m => (a -> m) -> t a -> m
    foldMap1' f = foldl1'map f (<>)

    -- | Right-associative fold of a structure.
    --
    -- In the case of lists, 'foldr1', when applied to a binary operator,
    -- and a list, reduces the list using the binary operator,
    -- from right to left:
    --
    -- > foldr1 f [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn1 `f` xn )...)
    --
    -- Note that, since the head of the resulting expression is produced by
    -- an application of the operator to the first element of the list,
    -- 'foldr1' can produce a terminating expression from an infinite list.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldr1 f = foldr1 f . 'toNonEmpty'@
    --
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 = foldr1map id

    -- | Right-associative fold of a structure, but with strict application of
    -- the operator.
    foldr1' :: (a -> a -> a) -> t a -> a
    foldr1' = foldr1'map id

    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, 'foldl1', when applied to a binary
    -- operator, and a list, reduces the list using the binary operator,
    -- from left to right:
    --
    -- > foldl1 f [x1, x2, ..., xn] == (...((x1 `f` x2) `f`...) `f` xn
    --
    -- Note that to produce the outermost application of the operator the
    -- entire input list must be traversed. This means that 'foldl1' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use 'foldl1'' instead of 'foldl1'. The reason for this is that latter does
    -- not force the "inner" results (e.g. @x1 \`f\` x2@ in the above example)
    -- before applying them to the operator (e.g. to @(\`f\` x3)@). This results
    -- in a thunk chain \(\mathcal{O}(n)\) elements long, which then must be
    -- evaluated from the outside-in.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldl1 f z = foldl1 f . 'toNonEmpty'@
    --
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 = foldl1map id

    -- | Left-associative fold of a structure but with strict application of
    -- the operator.
    --
    -- This ensures that each step of the fold is forced to weak head normal
    -- form before being applied, avoiding the collection of thunks that would
    -- otherwise occur. This is often what you want to strictly reduce a finite
    -- list to a single, monolithic result (e.g. 'length').
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldl1' f z = foldl1 f . 'toNonEmpty'@
    foldl1' :: (a -> a -> a) -> t a -> a
    foldl1' = foldl1'map id

    -- | List of elements of a structure, from left to right.
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . foldMap1 singleton

    -- | The largest element of a non-empty structure.
    maximum1 :: forall a . Ord a => t a -> a
    maximum1 = getMax . foldMap1 Max

    -- | The least element of a non-empty structure.
    minimum1 :: forall a . Ord a => t a -> a
    minimum1 = getMin . foldMap1 Min

    -- | The first element of a non-empty structure.
    head1 :: t a -> a
    head1 = getFirst . foldMap1 First

    -- | The last element of a non-empty structure.
    last1 :: t a -> a
    last1 = getLast . foldMap1 Last

    -- | For 'Functor's, @'foldr1map' f g = foldr1 g . 'fmap' g@.
    foldr1map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldr1map f g = foldr1map f g . toNonEmpty

    -- | For 'Functor's, @'foldl1'map' f g = foldl1' g . 'fmap' g@.
    foldl1'map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1'map f g xs = foldr1map f' (\x y z -> y $! SJust (x z)) xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g (f a) b

    -- | For 'Functor's, @'foldl1map' f g = foldl1 g . 'fmap' g@.
    foldl1map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1map f g = foldl1map f g . toNonEmpty

    -- | For 'Functor's, @'foldr1'map' f g = foldr1' g . 'fmap' g@.
    foldr1'map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldr1'map f g xs = foldl1map f' (\x y z -> y $! SJust (x z)) xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g (f a) b

-- Strict maybe, used to implement default foldl1'map etc.
data SMaybe a = SNothing | SJust !a

-- instances for Prelude types

instance Foldable1 NonEmpty where
    foldMap1 f (x :| xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

    toNonEmpty = id

    foldr1 f (x :| xs) = go x xs where
        go y [] = y
        go y (z : zs) = f y (go z zs)

    foldr1map g f (x :| xs) = go x xs where
        go y [] = g y
        go y (z : zs) = f (g y) (go z zs) 

    foldl1 f (x :| xs) = foldl f x xs
    foldl1map g f (x :| xs) = foldl f (g x) (map g xs)

    foldl1' f (x :| xs) = foldl' f x xs
    foldl1'map g f (x :| xs) = foldl' f (g x) (map g xs)

    head1 = NE.head
    last1 = NE.last

instance Foldable1 ((,) a) where
    foldMap1 f (_, y) = f y
    foldr1 _ (_, y) = y
    toNonEmpty (_, x) = x :| []
    minimum1 (_, x) = x
    maximum1 (_, x) = x
    head1 (_, x) = x
    last1 (_, x) = x

instance Foldable1 Dual where
    foldMap1 = coerce

instance Foldable1 Sum where
    foldMap1 = coerce

instance Foldable1 Product where
    foldMap1 = coerce

instance Foldable1 Min where
    foldMap1 = coerce

instance Foldable1 Max where
    foldMap1 = coerce

instance Foldable1 First where
    foldMap1 = coerce

instance Foldable1 Last where
    foldMap1 = coerce

deriving newtype instance (Foldable1 f) => Foldable1 (Mon.Alt f)
deriving newtype instance (Foldable1 f) => Foldable1 (Mon.Ap f)

-- Instances for GHC.Generics

instance Foldable1 V1 where
    foldMap1 _ x = case x of {}

instance Foldable1 Par1 where
    foldMap1 = coerce

deriving newtype instance Foldable1 f => Foldable1 (Rec1 f)

deriving newtype instance Foldable1 f => Foldable1 (M1 i c f)

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
    foldMap1 f (L1 x) = foldMap1 f x
    foldMap1 f (R1 y) = foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :*: g) where
    foldMap1 f (x :*: y) = foldMap1 f x <> foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :.: g) where
    foldMap1 f = foldMap1 (foldMap1 f) . unComp1

-- | Insert an @m@ between each pair of @t m@. 
--
-- >>> intercalate1 ", " $ "hello" :| ["how", "are", "you"]
-- "hello, how, are, you"
--
-- >>> intercalate1 ", " $ "hello" :| []
-- "hello"
--
-- >>> intercalate1 mempty $ "I" :| ["Am", "Fine", "You?"]
-- "IAmFineYou?"
--
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 sep = foldr1 (\a b -> a <> sep <> b)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 f = go . toNonEmpty
  where
    go (e:|es) =
      case es of
        []   -> return e
        x:xs -> f e =<< go (x:|xs)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 f t = foldlM f x xs
  where x:|xs = toNonEmpty t

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
maximum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
maximum1By cmp = foldl1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
minimum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimum1By cmp = foldl1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-- NonEmptyDList

newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  xs <> ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL #. (:|)

-- | Convert a dlist to a non-empty list
runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($[]) . unNEDL
{-# INLINE runNonEmptyDList #-}

instance Foldable1 Identity where
    foldMap1                = coerce
    foldr1 _                = coerce
    foldr1map g _           = coerce g
    toNonEmpty (Identity x) = x :| []

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
    foldMap1 f (Functor.Pair x y)    = foldMap1 f x <> foldMap1 f y
    foldr1map g f (Functor.Pair x y) = foldr (f . g) (foldr1map g f y) x

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
    foldMap1 f (Functor.InL x) = foldMap1 f x
    foldMap1 f (Functor.InR y) = foldMap1 f y

    foldr1 f (Functor.InL x) = foldr1 f x
    foldr1 f (Functor.InR y) = foldr1 f y

    foldr1map g f (Functor.InL x) = foldr1map g f x
    foldr1map g f (Functor.InR y) = foldr1map g f y

    toNonEmpty (Functor.InL x) = toNonEmpty x
    toNonEmpty (Functor.InR y) = toNonEmpty y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose
