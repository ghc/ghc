{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
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
import Control.Monad
import Data.Functor.Constant
import Data.Maybe (fromMaybe)
import Data.Monoid

#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#else
import Unsafe.Coerce
#endif

import Data.Semigroup (Arg(..))

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (K1(..))
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
import Data.Typeable
#endif

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
-- If the type is also a 'Bifunctor' instance, it should satisfy:
--
-- > 'bifoldMap' f g ≡ 'bifold' . 'bimap' f g
--
-- which implies that
--
-- > 'bifoldMap' f g . 'bimap' h i ≡ 'bifoldMap' (f . h) (g . i)
class Bifoldable p where
  -- | Combines the elements of a structure using a monoid.
  --
  -- @'bifold' ≡ 'bifoldMap' 'id' 'id'@
  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id
  {-# INLINE bifold #-}

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  --
  -- @'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'@
  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty
  {-# INLINE bifoldMap #-}

  -- | Combines the elements of a structure in a right associative manner. Given
  -- a hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'bifoldr' f g z ≡ 'foldr' ('either' f g) z . toEitherList@
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo #. f) (Endo #. g) t) z
  {-# INLINE bifoldr #-}

  -- | Combines the elments of a structure in a left associative manner. Given a
  -- hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'bifoldl' f g z ≡ 'foldl' (\acc -> 'either' (f acc) (g acc)) z .  toEitherList@
  --
  -- Note that if you want an efficient left-fold, you probably want to use
  -- 'bifoldl'' instead of 'bifoldl'. The reason is that the latter does not
  -- force the "inner" results, resulting in a thunk chain which then must be
  -- evaluated from the outside-in.
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f) (Dual . Endo . flip g) t)) z
  {-# INLINE bifoldl #-}

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bifoldr | bifoldMap #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Bifoldable
#endif

instance Bifoldable Arg where
  bifoldMap f g (Arg a b) = f a `mappend` g b

instance Bifoldable (,) where
  bifoldMap f g ~(a, b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a
  {-# INLINE bifoldMap #-}

instance Bifoldable Constant where
  bifoldMap f _ (Constant a) = f a
  {-# INLINE bifoldMap #-}

#if __GLASGOW_HASKELL__ >= 702
instance Bifoldable (K1 i) where
  bifoldMap f _ (K1 c) = f c
  {-# INLINE bifoldMap #-}
#endif

instance Bifoldable ((,,) x) where
  bifoldMap f g ~(_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,) x y) where
  bifoldMap f g ~(_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,) x y z) where
  bifoldMap f g ~(_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,,) x y z w) where
  bifoldMap f g ~(_,_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,,,) x y z w v) where
  bifoldMap f g ~(_,_,_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

#ifdef MIN_VERSION_tagged
instance Bifoldable Tagged where
  bifoldMap _ g (Tagged b) = g b
  {-# INLINE bifoldMap #-}
#endif

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b
  {-# INLINE bifoldMap #-}

-- | As 'bifoldr', but strict in the result of the reduction functions at each
-- step.
bifoldr' :: Bifoldable t => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0 where
  f' k x z = k $! f x z
  g' k x z = k $! g x z
{-# INLINE bifoldr' #-}

-- | A variant of 'bifoldr' that has no base case,
-- and thus may only be applied to non-empty structures.
bifoldr1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldr1 f xs = fromMaybe (error "bifoldr1: empty structure")
                  (bifoldr mbf mbf Nothing xs)
  where
    mbf x m = Just (case m of
                      Nothing -> x
                      Just y  -> f x y)
{-# INLINE bifoldr1 #-}

-- | Right associative monadic bifold over a structure.
bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0 where
  f' k x z = f x z >>= k
  g' k x z = g x z >>= k
{-# INLINE bifoldrM #-}

-- | As 'bifoldl', but strict in the result of the reduction functions at each
-- step.
--
-- This ensures that each step of the bifold is forced to weak head normal form
-- before being applied, avoiding the collection of thunks that would otherwise
-- occur. This is often what you want to strictly reduce a finite structure to
-- a single, monolithic result (e.g., 'bilength').
bifoldl':: Bifoldable t => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0 where
  f' x k z = k $! f z x
  g' x k z = k $! g z x
{-# INLINE bifoldl' #-}

-- | A variant of 'bifoldl' that has no base case,
-- and thus may only be applied to non-empty structures.
bifoldl1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldl1 f xs = fromMaybe (error "bifoldl1: empty structure")
                  (bifoldl mbf mbf Nothing xs)
  where
    mbf m y = Just (case m of
                      Nothing -> y
                      Just x  -> f x y)
{-# INLINe bifoldl1 #-}

-- | Left associative monadic bifold over a structure.
bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr f' g' return xs z0 where
  f' x k z = f z x >>= k
  g' x k z = g z x >>= k
{-# INLINE bifoldlM #-}

-- | Map each element of a structure using one of two actions, evaluate these
-- actions from left to right, and ignore the results. For a version that
-- doesn't ignore the results, see 'Data.Bitraversable.bitraverse'.
bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())
{-# INLINE bitraverse_ #-}

-- | As 'bitraverse_', but with the structure as the primary argument. For a
-- version that doesn't ignore the results, see 'Data.Bitraversable.bifor'.
--
-- >>> > bifor_ ('a', "bc") print (print . reverse)
-- 'a'
-- "cb"
bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t
{-# INLINE bifor_ #-}

-- | As 'Data.Bitraversable.bimapM', but ignores the results of the functions,
-- merely performing the "actions".
bimapM_:: (Bifoldable t, Monad m) => (a -> m c) -> (b -> m d) -> t a b -> m ()
bimapM_ f g = bifoldr ((>>) . f) ((>>) . g) (return ())
{-# INLINE bimapM_ #-}

-- | As 'bimapM_', but with the structure as the primary argument.
biforM_ :: (Bifoldable t, Monad m) => t a b ->  (a -> m c) -> (b -> m d) -> m ()
biforM_ t f g = bimapM_ f g t
{-# INLINE biforM_ #-}

-- | As 'Data.Bitraversable.bisequenceA', but ignores the results of the actions.
bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bifoldr (*>) (*>) (pure ())
{-# INLINE bisequenceA_ #-}

-- | Evaluate each action in the structure from left to right, and ignore the
-- results. For a version that doesn't ignore the results, see
-- 'Data.Bitraversable.bisequence'.
bisequence_ :: (Bifoldable t, Monad m) => t (m a) (m b) -> m ()
bisequence_ = bifoldr (>>) (>>) (return ())
{-# INLINE bisequence_ #-}

-- | The sum of a collection of actions, generalizing 'biconcat'.
biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty
{-# INLINE biasum #-}

-- | The sum of a collection of actions, generalizing 'biconcat'.
bimsum :: (Bifoldable t, MonadPlus m) => t (m a) (m a) -> m a
bimsum = bifoldr mplus mplus mzero
{-# INLINE bimsum #-}

-- | Collects the list of elements of a structure, from left to right.
biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []
{-# INLINE biList #-}

-- | Test whether the structure is empty.
binull :: Bifoldable t => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True
{-# INLINE binull #-}

-- | Returns the size/length of a finite structure as an 'Int'.
bilength :: Bifoldable t => t a b -> Int
bilength = bifoldl' (\c _ -> c+1) (\c _ -> c+1) 0
{-# INLINE bilength #-}

-- | Does the element occur in the structure?
bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)
{-# INLINE bielem #-}

-- | Reduces a structure of lists to the concatenation of those lists.
biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold
{-# INLINE biconcat #-}

newtype Max a = Max {getMax :: Maybe a}
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

  {-# INLINE mappend #-}
  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

  {-# INLINE mappend #-}
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

-- | The largest element of a non-empty structure.
bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = fromMaybe (error "bimaximum: empty structure") .
    getMax . bifoldMap mj mj
  where mj = Max #. (Just :: a -> Maybe a)
{-# INLINE bimaximum #-}

-- | The least element of a non-empty structure.
biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = fromMaybe (error "biminimum: empty structure") .
    getMin . bifoldMap mj mj
  where mj = Min #. (Just :: a -> Maybe a)
{-# INLINE biminimum #-}

-- | The 'bisum' function computes the sum of the numbers of a structure.
bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum #. bifoldMap Sum Sum
{-# INLINE bisum #-}

-- | The 'biproduct' function computes the product of the numbers of a
-- structure.
biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct #. bifoldMap Product Product
{-# INLINE biproduct #-}

-- | Given a means of mapping the elements of a structure to lists, computes the
-- concatenation of all such lists in order.
biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap
{-# INLINE biconcatMap #-}

-- | 'biand' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
biand :: Bifoldable t => t Bool Bool -> Bool
biand = getAll #. bifoldMap All All
{-# INLINE biand #-}

-- | 'bior' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
bior :: Bifoldable t => t Bool Bool -> Bool
bior = getAny #. bifoldMap Any Any
{-# INLINE bior #-}

-- | Determines whether any element of the structure satisfies the appropriate
-- predicate.
biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny #. bifoldMap (Any . p) (Any . q)
{-# INLINE biany #-}

-- | Determines whether all elements of the structure satisfy the appropriate
-- predicate.
biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll #. bifoldMap (All . p) (All . q)
{-# INLINE biall #-}

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
bimaximumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
bimaximumBy cmp = bifoldr1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y
{-# INLINE bimaximumBy #-}

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
biminimumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = bifoldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x
{-# INLINE biminimumBy #-}

-- | 'binotElem' is the negation of 'bielem'.
binotElem :: (Bifoldable t, Eq a) => a -> t a a-> Bool
binotElem x =  not . bielem x
{-# INLINE binotElem #-}

-- | The 'bifind' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
bifind :: Bifoldable t => (a -> Bool) -> t a a -> Maybe a
bifind p = getFirst . bifoldMap finder finder
  where finder x = First (if p x then Just x else Nothing)
{-# INLINE bifind #-}

-- See Note [Function coercion]
#if MIN_VERSION_base(4,7,0)
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
#else
(#.) :: (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = unsafeCoerce
#endif
{-# INLINE (#.) #-}

{-
Note [Function coercion]
~~~~~~~~~~~~~~~~~~~~~~~~

Several functions here use (#.) instead of (.) to avoid potential efficiency
problems relating to #7542. The problem, in a nutshell:

If N is a newtype constructor, then N x will always have the same
representation as x (something similar applies for a newtype deconstructor).
However, if f is a function,

N . f = \x -> N (f x)

This looks almost the same as f, but the eta expansion lifts it--the lhs could
be _|_, but the rhs never is. This can lead to very inefficient code.  Thus we
steal a technique from Shachaf and Edward Kmett and adapt it to the current
(rather clean) setting. Instead of using  N . f,  we use  N .## f, which is
just

coerce f `asTypeOf` (N . f)

That is, we just *pretend* that f has the right type, and thanks to the safety
of coerce, the type checker guarantees that nothing really goes wrong. We still
have to be a bit careful, though: remember that #. completely ignores the
*value* of its left operand.
-}
