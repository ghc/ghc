{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifoldable
-- Copyright   :  (C) 2011-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- @since 4.10.0.0
----------------------------------------------------------------------------
module Data.Bifoldable
  ( Bifoldable(..)
  , bifoldr'
  , bifoldr1
  , bifoldrM
  , bifoldl'
  , bifoldl1
  , bifoldlM
  , bitraverse_
  , bifor_
  , bimapM_
  , biforM_
  , bimsum
  , bisequenceA_
  , bisequence_
  , biasum
  , biList
  , binull
  , bilength
  , bielem
  , bimaximum
  , biminimum
  , bisum
  , biproduct
  , biconcat
  , biconcatMap
  , biand
  , bior
  , biany
  , biall
  , bimaximumBy
  , biminimumBy
  , binotElem
  , bifind
  ) where

import Control.Applicative
import Data.Functor.Utils (Max(..), Min(..), (#.))
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics (K1(..))

-- | 'Bifoldable' identifies foldable structures with two different varieties
-- of elements (as opposed to 'Foldable', which has one variety of element).
-- Common examples are 'Either' and '(,)':
--
-- > instance Bifoldable Either where
-- >   bifoldMap f _ (Left  a) = f a
-- >   bifoldMap _ g (Right b) = g b
-- >
-- > instance Bifoldable (,) where
-- >   bifoldr f g z (a, b) = f a (g b z)
--
-- A minimal 'Bifoldable' definition consists of either 'bifoldMap' or
-- 'bifoldr'. When defining more than this minimal set, one should ensure
-- that the following identities hold:
--
-- @
-- 'bifold' ≡ 'bifoldMap' 'id' 'id'
-- 'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'
-- 'bifoldr' f g z t ≡ 'appEndo' ('bifoldMap' (Endo . f) (Endo . g) t) z
-- @
--
-- If the type is also a 'Data.Bifunctor.Bifunctor' instance, it should satisfy:
--
-- > 'bifoldMap' f g ≡ 'bifold' . 'bimap' f g
--
-- which implies that
--
-- > 'bifoldMap' f g . 'bimap' h i ≡ 'bifoldMap' (f . h) (g . i)
--
-- @since 4.10.0.0
class Bifoldable p where
  {-# MINIMAL bifoldr | bifoldMap #-}

  -- | Combines the elements of a structure using a monoid.
  --
  -- @'bifold' ≡ 'bifoldMap' 'id' 'id'@
  --
  -- @since 4.10.0.0
  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  --
  -- @'bifoldMap' f g
  --     ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'@
  --
  -- @since 4.10.0.0
  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty

  -- | Combines the elements of a structure in a right associative manner.
  -- Given a hypothetical function @toEitherList :: p a b -> [Either a b]@
  -- yielding a list of all elements of a structure in order, the following
  -- would hold:
  --
  -- @'bifoldr' f g z ≡ 'foldr' ('either' f g) z . toEitherList@
  --
  -- @since 4.10.0.0
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo #. f) (Endo #. g) t) z

  -- | Combines the elements of a structure in a left associative manner. Given
  -- a hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'bifoldl' f g z
  --     ≡ 'foldl' (\acc -> 'either' (f acc) (g acc)) z . toEitherList@
  --
  -- Note that if you want an efficient left-fold, you probably want to use
  -- 'bifoldl'' instead of 'bifoldl'. The reason is that the latter does not
  -- force the "inner" results, resulting in a thunk chain which then must be
  -- evaluated from the outside-in.
  --
  -- @since 4.10.0.0
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f)
                                                (Dual . Endo . flip g) t)) z

-- | @since 4.10.0.0
instance Bifoldable (,) where
  bifoldMap f g ~(a, b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a

-- | @since 4.10.0.0
instance Bifoldable (K1 i) where
  bifoldMap f _ (K1 c) = f c

-- | @since 4.10.0.0
instance Bifoldable ((,,) x) where
  bifoldMap f g ~(_,a,b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable ((,,,) x y) where
  bifoldMap f g ~(_,_,a,b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable ((,,,,) x y z) where
  bifoldMap f g ~(_,_,_,a,b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable ((,,,,,) x y z w) where
  bifoldMap f g ~(_,_,_,_,a,b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable ((,,,,,,) x y z w v) where
  bifoldMap f g ~(_,_,_,_,_,a,b) = f a `mappend` g b

-- | @since 4.10.0.0
instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

-- | As 'bifoldr', but strict in the result of the reduction functions at each
-- step.
--
-- @since 4.10.0.0
bifoldr' :: Bifoldable t => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0 where
  f' k x z = k $! f x z
  g' k x z = k $! g x z

-- | A variant of 'bifoldr' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @since 4.10.0.0
bifoldr1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldr1 f xs = fromMaybe (error "bifoldr1: empty structure")
                  (bifoldr mbf mbf Nothing xs)
  where
    mbf x m = Just (case m of
                      Nothing -> x
                      Just y  -> f x y)

-- | Right associative monadic bifold over a structure.
--
-- @since 4.10.0.0
bifoldrM :: (Bifoldable t, Monad m)
         => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0 where
  f' k x z = f x z >>= k
  g' k x z = g x z >>= k

-- | As 'bifoldl', but strict in the result of the reduction functions at each
-- step.
--
-- This ensures that each step of the bifold is forced to weak head normal form
-- before being applied, avoiding the collection of thunks that would otherwise
-- occur. This is often what you want to strictly reduce a finite structure to
-- a single, monolithic result (e.g., 'bilength').
--
-- @since 4.10.0.0
bifoldl':: Bifoldable t => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0 where
  f' x k z = k $! f z x
  g' x k z = k $! g z x

-- | A variant of 'bifoldl' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @since 4.10.0.0
bifoldl1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldl1 f xs = fromMaybe (error "bifoldl1: empty structure")
                  (bifoldl mbf mbf Nothing xs)
  where
    mbf m y = Just (case m of
                      Nothing -> y
                      Just x  -> f x y)

-- | Left associative monadic bifold over a structure.
--
-- @since 4.10.0.0
bifoldlM :: (Bifoldable t, Monad m)
         => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr f' g' return xs z0 where
  f' x k z = f z x >>= k
  g' x k z = g z x >>= k

-- | Map each element of a structure using one of two actions, evaluate these
-- actions from left to right, and ignore the results. For a version that
-- doesn't ignore the results, see 'Data.Bitraversable.bitraverse'.
--
-- @since 4.10.0.0
bitraverse_ :: (Bifoldable t, Applicative f)
            => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())

-- | As 'bitraverse_', but with the structure as the primary argument. For a
-- version that doesn't ignore the results, see 'Data.Bitraversable.bifor'.
--
-- >>> > bifor_ ('a', "bc") print (print . reverse)
-- 'a'
-- "cb"
--
-- @since 4.10.0.0
bifor_ :: (Bifoldable t, Applicative f)
       => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t

-- | Alias for 'bitraverse_'.
--
-- @since 4.10.0.0
bimapM_ :: (Bifoldable t, Applicative f)
        => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = bitraverse_

-- | Alias for 'bifor_'.
--
-- @since 4.10.0.0
biforM_ :: (Bifoldable t, Applicative f)
        => t a b ->  (a -> f c) -> (b -> f d) -> f ()
biforM_ = bifor_

-- | Alias for 'bisequence_'.
--
-- @since 4.10.0.0
bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bisequence_

-- | Evaluate each action in the structure from left to right, and ignore the
-- results. For a version that doesn't ignore the results, see
-- 'Data.Bitraversable.bisequence'.
--
-- @since 4.10.0.0
bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bifoldr (*>) (*>) (pure ())

-- | The sum of a collection of actions, generalizing 'biconcat'.
--
-- @since 4.10.0.0
biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty

-- | Alias for 'biasum'.
--
-- @since 4.10.0.0
bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = biasum

-- | Collects the list of elements of a structure, from left to right.
--
-- @since 4.10.0.0
biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []

-- | Test whether the structure is empty.
--
-- @since 4.10.0.0
binull :: Bifoldable t => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.
--
-- @since 4.10.0.0
bilength :: Bifoldable t => t a b -> Int
bilength = bifoldl' (\c _ -> c+1) (\c _ -> c+1) 0

-- | Does the element occur in the structure?
--
-- @since 4.10.0.0
bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)

-- | Reduces a structure of lists to the concatenation of those lists.
--
-- @since 4.10.0.0
biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold

-- | The largest element of a non-empty structure.
--
-- @since 4.10.0.0
bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = fromMaybe (error "bimaximum: empty structure") .
    getMax . bifoldMap mj mj
  where mj = Max #. (Just :: a -> Maybe a)

-- | The least element of a non-empty structure.
--
-- @since 4.10.0.0
biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = fromMaybe (error "biminimum: empty structure") .
    getMin . bifoldMap mj mj
  where mj = Min #. (Just :: a -> Maybe a)

-- | The 'bisum' function computes the sum of the numbers of a structure.
--
-- @since 4.10.0.0
bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum #. bifoldMap Sum Sum

-- | The 'biproduct' function computes the product of the numbers of a
-- structure.
--
-- @since 4.10.0.0
biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct #. bifoldMap Product Product

-- | Given a means of mapping the elements of a structure to lists, computes the
-- concatenation of all such lists in order.
--
-- @since 4.10.0.0
biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

-- | 'biand' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
--
-- @since 4.10.0.0
biand :: Bifoldable t => t Bool Bool -> Bool
biand = getAll #. bifoldMap All All

-- | 'bior' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
--
-- @since 4.10.0.0
bior :: Bifoldable t => t Bool Bool -> Bool
bior = getAny #. bifoldMap Any Any

-- | Determines whether any element of the structure satisfies its appropriate
-- predicate argument.
--
-- @since 4.10.0.0
biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny #. bifoldMap (Any . p) (Any . q)

-- | Determines whether all elements of the structure satisfy their appropriate
-- predicate argument.
--
-- @since 4.10.0.0
biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll #. bifoldMap (All . p) (All . q)

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
--
-- @since 4.10.0.0
bimaximumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
bimaximumBy cmp = bifoldr1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
--
-- @since 4.10.0.0
biminimumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = bifoldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-- | 'binotElem' is the negation of 'bielem'.
--
-- @since 4.10.0.0
binotElem :: (Bifoldable t, Eq a) => a -> t a a-> Bool
binotElem x =  not . bielem x

-- | The 'bifind' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
--
-- @since 4.10.0.0
bifind :: Bifoldable t => (a -> Bool) -> t a a -> Maybe a
bifind p = getFirst . bifoldMap finder finder
  where finder x = First (if p x then Just x else Nothing)
