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

-- $setup
-- >>> import Prelude
-- >>> import Data.Char
-- >>> import Data.Monoid (Product (..), Sum (..))
-- >>> data BiList a b = BiList [a] [b]
-- >>> instance Bifoldable BiList where bifoldr f g z (BiList as bs) = foldr f (foldr g z bs) as

-- | 'Bifoldable' identifies foldable structures with two different varieties
-- of elements (as opposed to 'Foldable', which has one variety of element).
-- Common examples are 'Either' and @(,)@:
--
-- > instance Bifoldable Either where
-- >   bifoldMap f _ (Left  a) = f a
-- >   bifoldMap _ g (Right b) = g b
-- >
-- > instance Bifoldable (,) where
-- >   bifoldr f g z (a, b) = f a (g b z)
--
-- Some examples below also use the following BiList to showcase empty
-- Bifoldable behaviors when relevant ('Either' and '(,)' containing always exactly
-- resp. 1 and 2 elements):
--
-- > data BiList a b = BiList [a] [b]
-- >
-- > instance Bifoldable BiList where
-- >   bifoldr f g z (BiList as bs) = foldr f (foldr g z bs) as
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
-- @
-- 'bifoldMap' f g ≡ 'bifold' . 'Data.Bifunctor.bimap' f g
-- @
--
-- which implies that
--
-- @
-- 'bifoldMap' f g . 'Data.Bifunctor.bimap' h i ≡ 'bifoldMap' (f . h) (g . i)
-- @
--
-- @since 4.10.0.0
class Bifoldable p where
  {-# MINIMAL bifoldr | bifoldMap #-}

  -- | Combines the elements of a structure using a monoid.
  --
  -- @'bifold' ≡ 'bifoldMap' 'id' 'id'@
  --
  -- ==== __Examples__
  --
  -- Basic usage:
  --
  -- >>> bifold (Right [1, 2, 3])
  -- [1,2,3]
  --
  -- >>> bifold (Left [5, 6])
  -- [5,6]
  --
  -- >>> bifold ([1, 2, 3], [4, 5])
  -- [1,2,3,4,5]
  --
  -- >>> bifold (Product 6, Product 7)
  -- Product {getProduct = 42}
  --
  -- >>> bifold (Sum 6, Sum 7)
  -- Sum {getSum = 13}
  --
  -- @since 4.10.0.0
  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  --
  -- @'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'@
  --
  -- ==== __Examples__
  --
  -- Basic usage:
  --
  -- >>> bifoldMap (take 3) (fmap digitToInt) ([1..], "89")
  -- [1,2,3,8,9]
  --
  -- >>> bifoldMap (take 3) (fmap digitToInt) (Left [1..])
  -- [1,2,3]
  --
  -- >>> bifoldMap (take 3) (fmap digitToInt) (Right "89")
  -- [8,9]
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
  -- ==== __Examples__
  --
  -- Basic usage:
  --
  -- @
  -- > bifoldr (+) (*) 3 (5, 7)
  -- 26 -- 5 + (7 * 3)
  --
  -- > bifoldr (+) (*) 3 (7, 5)
  -- 22 -- 7 + (5 * 3)
  --
  -- > bifoldr (+) (*) 3 (Right 5)
  -- 15 -- 5 * 3
  --
  -- > bifoldr (+) (*) 3 (Left 5)
  -- 8 -- 5 + 3
  -- @
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
  -- ==== __Examples__
  --
  -- Basic usage:
  --
  -- @
  -- > bifoldl (+) (*) 3 (5, 7)
  -- 56 -- (5 + 3) * 7
  --
  -- > bifoldl (+) (*) 3 (7, 5)
  -- 50 -- (7 + 3) * 5
  --
  -- > bifoldl (+) (*) 3 (Right 5)
  -- 15 -- 5 * 3
  --
  -- > bifoldl (+) (*) 3 (Left 5)
  -- 8 -- 5 + 3
  -- @
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifoldr1 (+) (5, 7)
-- 12
--
-- >>> bifoldr1 (+) (Right 7)
-- 7
--
-- >>> bifoldr1 (+) (Left 5)
-- 5
--
-- @
-- > bifoldr1 (+) (BiList [1, 2] [3, 4])
-- 10 -- 1 + (2 + (3 + 4))
-- @
--
-- >>> bifoldr1 (+) (BiList [1, 2] [])
-- 3
--
-- On empty structures, this function throws an exception:
--
-- >>> bifoldr1 (+) (BiList [] [])
-- *** Exception: bifoldr1: empty structure
-- ...
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifoldl1 (+) (5, 7)
-- 12
--
-- >>> bifoldl1 (+) (Right 7)
-- 7
--
-- >>> bifoldl1 (+) (Left 5)
-- 5
--
-- @
-- > bifoldl1 (+) (BiList [1, 2] [3, 4])
-- 10 -- ((1 + 2) + 3) + 4
-- @
--
-- >>> bifoldl1 (+) (BiList [1, 2] [])
-- 3
--
-- On empty structures, this function throws an exception:
--
-- >>> bifoldl1 (+) (BiList [] [])
-- *** Exception: bifoldl1: empty structure
-- ...
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifoldlM (\a b -> print b >> pure a) (\a c -> print (show c) >> pure a) 42 ("Hello", True)
-- "Hello"
-- "True"
-- 42
--
-- >>> bifoldlM (\a b -> print b >> pure a) (\a c -> print (show c) >> pure a) 42 (Right True)
-- "True"
-- 42
--
-- >>> bifoldlM (\a b -> print b >> pure a) (\a c -> print (show c) >> pure a) 42 (Left "Hello")
-- "Hello"
-- 42
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bitraverse_ print (print . show) ("Hello", True)
-- "Hello"
-- "True"
--
-- >>> bitraverse_ print (print . show) (Right True)
-- "True"
--
-- >>> bitraverse_ print (print . show) (Left "Hello")
-- "Hello"
--
-- @since 4.10.0.0
bitraverse_ :: (Bifoldable t, Applicative f)
            => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())

-- | As 'bitraverse_', but with the structure as the primary argument. For a
-- version that doesn't ignore the results, see 'Data.Bitraversable.bifor'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifor_ ("Hello", True) print (print . show)
-- "Hello"
-- "True"
--
-- >>> bifor_ (Right True) print (print . show)
-- "True"
--
-- >>> bifor_ (Left "Hello") print (print . show)
-- "Hello"
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bisequence_ (print "Hello", print "World")
-- "Hello"
-- "World"
--
-- >>> bisequence_ (Left (print "Hello"))
-- "Hello"
--
-- >>> bisequence_ (Right (print "World"))
-- "World"
--
-- @since 4.10.0.0
bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bifoldr (*>) (*>) (pure ())

-- | The sum of a collection of actions, generalizing 'biconcat'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biasum (Nothing, Nothing)
-- Nothing
--
-- >>> biasum (Nothing, Just 42)
-- Just 42
--
-- >>> biasum (Just 18, Nothing)
-- Just 18
--
-- >>> biasum (Just 18, Just 42)
-- Just 18
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biList (18, 42)
-- [18,42]
--
-- >>> biList (Left 18)
-- [18]
--
-- @since 4.10.0.0
biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []

-- | Test whether the structure is empty.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> binull (18, 42)
-- False
--
-- >>> binull (Right 42)
-- False
--
-- >>> binull (BiList [] [])
-- True
--
-- @since 4.10.0.0
binull :: Bifoldable t => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bilength (True, 42)
-- 2
--
-- >>> bilength (Right 42)
-- 1
--
-- >>> bilength (BiList [1,2,3] [4,5])
-- 5
--
-- >>> bilength (BiList [] [])
-- 0
--
-- On infinite structures, this function hangs:
--
-- @
-- > bilength (BiList [1..] [])
-- * Hangs forever *
-- @
--
-- @since 4.10.0.0
bilength :: Bifoldable t => t a b -> Int
bilength = bifoldl' (\c _ -> c+1) (\c _ -> c+1) 0

-- | Does the element occur in the structure?
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bielem 42 (17, 42)
-- True
--
-- >>> bielem 42 (17, 43)
-- False
--
-- >>> bielem 42 (Left 42)
-- True
--
-- >>> bielem 42 (Right 13)
-- False
--
-- >>> bielem 42 (BiList [1..5] [1..100])
-- True
--
-- >>> bielem 42 (BiList [1..5] [1..41])
-- False
--
-- @since 4.10.0.0
bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)

-- | Reduces a structure of lists to the concatenation of those lists.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biconcat ([1, 2, 3], [4, 5])
-- [1,2,3,4,5]
--
-- >>> biconcat (Left [1, 2, 3])
-- [1,2,3]
--
-- >>> biconcat (BiList [[1, 2, 3, 4, 5], [6, 7, 8]] [[9]])
-- [1,2,3,4,5,6,7,8,9]
--
-- @since 4.10.0.0
biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold

-- | The largest element of a non-empty structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bimaximum (42, 17)
-- 42
--
-- >>> bimaximum (Right 42)
-- 42
--
-- >>> bimaximum (BiList [13, 29, 4] [18, 1, 7])
-- 29
--
-- >>> bimaximum (BiList [13, 29, 4] [])
-- 29
--
-- On empty structures, this function throws an exception:
--
-- >>> bimaximum (BiList [] [])
-- *** Exception: bimaximum: empty structure
-- ...
--
-- @since 4.10.0.0
bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = fromMaybe (error "bimaximum: empty structure") .
    getMax . bifoldMap mj mj
  where mj = Max #. (Just :: a -> Maybe a)

-- | The least element of a non-empty structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biminimum (42, 17)
-- 17
--
-- >>> biminimum (Right 42)
-- 42
--
-- >>> biminimum (BiList [13, 29, 4] [18, 1, 7])
-- 1
--
-- >>> biminimum (BiList [13, 29, 4] [])
-- 4
--
-- On empty structures, this function throws an exception:
--
-- >>> biminimum (BiList [] [])
-- *** Exception: biminimum: empty structure
-- ...
--
-- @since 4.10.0.0
biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = fromMaybe (error "biminimum: empty structure") .
    getMin . bifoldMap mj mj
  where mj = Min #. (Just :: a -> Maybe a)

-- | The 'bisum' function computes the sum of the numbers of a structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bisum (42, 17)
-- 59
--
-- >>> bisum (Right 42)
-- 42
--
-- >>> bisum (BiList [13, 29, 4] [18, 1, 7])
-- 72
--
-- >>> bisum (BiList [13, 29, 4] [])
-- 46
--
-- >>> bisum (BiList [] [])
-- 0
--
-- @since 4.10.0.0
bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum #. bifoldMap Sum Sum

-- | The 'biproduct' function computes the product of the numbers of a
-- structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biproduct (42, 17)
-- 714
--
-- >>> biproduct (Right 42)
-- 42
--
-- >>> biproduct (BiList [13, 29, 4] [18, 1, 7])
-- 190008
--
-- >>> biproduct (BiList [13, 29, 4] [])
-- 1508
--
-- >>> biproduct (BiList [] [])
-- 1
--
-- @since 4.10.0.0
biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct #. bifoldMap Product Product

-- | Given a means of mapping the elements of a structure to lists, computes the
-- concatenation of all such lists in order.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biconcatMap (take 3) (fmap digitToInt) ([1..], "89")
-- [1,2,3,8,9]
--
-- >>> biconcatMap (take 3) (fmap digitToInt) (Left [1..])
-- [1,2,3]
--
-- >>> biconcatMap (take 3) (fmap digitToInt) (Right "89")
-- [8,9]
--
-- @since 4.10.0.0
biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

-- | 'biand' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biand (True, False)
-- False
--
-- >>> biand (True, True)
-- True
--
-- >>> biand (Left True)
-- True
--
-- Empty structures yield 'True':
--
-- >>> biand (BiList [] [])
-- True
--
-- A 'False' value finitely far from the left end yields 'False' (short circuit):
--
-- >>> biand (BiList [True, True, False, True] (repeat True))
-- False
--
-- A 'False' value infinitely far from the left end hangs:
--
-- @
-- > biand (BiList (repeat True) [False])
-- * Hangs forever *
-- @
--
-- An infinitely 'True' value hangs:
--
-- @
-- > biand (BiList (repeat True) [])
-- * Hangs forever *
-- @
--
-- @since 4.10.0.0
biand :: Bifoldable t => t Bool Bool -> Bool
biand = getAll #. bifoldMap All All

-- | 'bior' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bior (True, False)
-- True
--
-- >>> bior (False, False)
-- False
--
-- >>> bior (Left True)
-- True
--
-- Empty structures yield 'False':
--
-- >>> bior (BiList [] [])
-- False
--
-- A 'True' value finitely far from the left end yields 'True' (short circuit):
--
-- >>> bior (BiList [False, False, True, False] (repeat False))
-- True
--
-- A 'True' value infinitely far from the left end hangs:
--
-- @
-- > bior (BiList (repeat False) [True])
-- * Hangs forever *
-- @
--
-- An infinitely 'False' value hangs:
--
-- @
-- > bior (BiList (repeat False) [])
-- * Hangs forever *
-- @
--
-- @since 4.10.0.0
bior :: Bifoldable t => t Bool Bool -> Bool
bior = getAny #. bifoldMap Any Any

-- | Determines whether any element of the structure satisfies its appropriate
-- predicate argument. Empty structures yield 'False'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biany even isDigit (27, 't')
-- False
--
-- >>> biany even isDigit (27, '8')
-- True
--
-- >>> biany even isDigit (26, 't')
-- True
--
-- >>> biany even isDigit (Left 27)
-- False
--
-- >>> biany even isDigit (Left 26)
-- True
--
-- >>> biany even isDigit (BiList [27, 53] ['t', '8'])
-- True
--
-- Empty structures yield 'False':
--
-- >>> biany even isDigit (BiList [] [])
-- False
--
-- @since 4.10.0.0
biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny #. bifoldMap (Any . p) (Any . q)

-- | Determines whether all elements of the structure satisfy their appropriate
-- predicate argument. Empty structures yield 'True'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biall even isDigit (27, 't')
-- False
--
-- >>> biall even isDigit (26, '8')
-- True
--
-- >>> biall even isDigit (Left 27)
-- False
--
-- >>> biall even isDigit (Left 26)
-- True
--
-- >>> biall even isDigit (BiList [26, 52] ['3', '8'])
-- True
--
-- Empty structures yield 'True':
--
-- >>> biall even isDigit (BiList [] [])
-- True
--
-- @since 4.10.0.0
biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll #. bifoldMap (All . p) (All . q)

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bimaximumBy compare (42, 17)
-- 42
--
-- >>> bimaximumBy compare (Left 17)
-- 17
--
-- >>> bimaximumBy compare (BiList [42, 17, 23] [-5, 18])
-- 42
--
-- On empty structures, this function throws an exception:
--
-- >>> bimaximumBy compare (BiList [] [])
-- *** Exception: bifoldr1: empty structure
-- ...
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
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> biminimumBy compare (42, 17)
-- 17
--
-- >>> biminimumBy compare (Left 17)
-- 17
--
-- >>> biminimumBy compare (BiList [42, 17, 23] [-5, 18])
-- -5
--
-- On empty structures, this function throws an exception:
--
-- >>> biminimumBy compare (BiList [] [])
-- *** Exception: bifoldr1: empty structure
-- ...
--
-- @since 4.10.0.0
biminimumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = bifoldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-- | 'binotElem' is the negation of 'bielem'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> binotElem 42 (17, 42)
-- False
--
-- >>> binotElem 42 (17, 43)
-- True
--
-- >>> binotElem 42 (Left 42)
-- False
--
-- >>> binotElem 42 (Right 13)
-- True
--
-- >>> binotElem 42 (BiList [1..5] [1..100])
-- False
--
-- >>> binotElem 42 (BiList [1..5] [1..41])
-- True
--
-- @since 4.10.0.0
binotElem :: (Bifoldable t, Eq a) => a -> t a a-> Bool
binotElem x =  not . bielem x

-- | The 'bifind' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifind even (27, 53)
-- Nothing
--
-- >>> bifind even (27, 52)
-- Just 52
--
-- >>> bifind even (26, 52)
-- Just 26
--
-- Empty structures always yield 'Nothing':
--
-- >>> bifind even (BiList [] [])
-- Nothing
--
-- @since 4.10.0.0
bifind :: Bifoldable t => (a -> Bool) -> t a a -> Maybe a
bifind p = getFirst . bifoldMap finder finder
  where finder x = First (if p x then Just x else Nothing)
