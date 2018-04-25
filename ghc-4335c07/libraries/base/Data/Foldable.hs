{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable
-- Copyright   :  Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be folded to a summary value.
--
-----------------------------------------------------------------------------

module Data.Foldable (
    Foldable(..),
    -- * Special biased folds
    foldrM,
    foldlM,
    -- * Folding actions
    -- ** Applicative actions
    traverse_,
    for_,
    sequenceA_,
    asum,
    -- ** Monadic actions
    mapM_,
    forM_,
    sequence_,
    msum,
    -- * Specialized folds
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    -- * Searches
    notElem,
    find
    ) where

import Data.Bool
import Data.Either
import Data.Eq
import Data.Functor.Utils (Max(..), Min(..), (#.))
import qualified GHC.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy

import GHC.Arr  ( Array(..), elems, numElements,
                  foldlElems, foldrElems,
                  foldlElems', foldrElems',
                  foldl1Elems, foldr1Elems)
import GHC.Base hiding ( foldr )
import GHC.Generics
import GHC.Num  ( Num(..) )

infix  4 `elem`, `notElem`

-- | Data structures that can be folded.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Foldable Tree where
-- >    foldMap f Empty = mempty
-- >    foldMap f (Leaf x) = f x
-- >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
--
-- This is suitable even for abstract types, as the monoid is assumed
-- to satisfy the monoid laws.  Alternatively, one could define @foldr@:
--
-- > instance Foldable Tree where
-- >    foldr f z Empty = z
-- >    foldr f z (Leaf x) = f x z
-- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
--
-- @Foldable@ instances are expected to satisfy the following laws:
--
-- > foldr f z t = appEndo (foldMap (Endo . f) t ) z
--
-- > foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
--
-- > fold = foldMap id
--
-- > length = getSum . foldMap (Sum . const  1)
--
-- @sum@, @product@, @maximum@, and @minimum@ should all be essentially
-- equivalent to @foldMap@ forms, such as
--
-- > sum = getSum . foldMap Sum
--
-- but may be less defined.
--
-- If the type is also a 'Functor' instance, it should satisfy
--
-- > foldMap f = fold . fmap f
--
-- which implies that
--
-- > foldMap f . fmap g = foldMap (f . g)

class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Combine the elements of a structure using a monoid.
    fold :: Monoid m => t m -> m
    fold = foldMap id

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
    foldMap :: Monoid m => (a -> m) -> t a -> m
    {-# INLINE foldMap #-}
    -- This INLINE allows more list functions to fuse. See Trac #9848.
    foldMap f = foldr (mappend . f) mempty

    -- | Right-associative fold of a structure.
    --
    -- In the case of lists, 'foldr', when applied to a binary operator, a
    -- starting value (typically the right-identity of the operator), and a
    -- list, reduces the list using the binary operator, from right to left:
    --
    -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
    --
    -- Note that, since the head of the resulting expression is produced by
    -- an application of the operator to the first element of the list,
    -- 'foldr' can produce a terminating expression from an infinite list.
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to,
    --
    -- @foldr f z = 'List.foldr' f z . 'toList'@
    --
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    -- | Right-associative fold of a structure, but with strict application of
    -- the operator.
    --
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z0 xs = foldl f' id xs z0
      where f' k x z = k $! f x z

    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, 'foldl', when applied to a binary
    -- operator, a starting value (typically the left-identity of the operator),
    -- and a list, reduces the list using the binary operator, from left to
    -- right:
    --
    -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
    --
    -- Note that to produce the outermost application of the operator the
    -- entire input list must be traversed. This means that 'foldl'' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use 'foldl'' instead of 'foldl'. The reason for this is that latter does
    -- not force the "inner" results (e.g. @z `f` x1@ in the above example)
    -- before applying them to the operator (e.g. to @(`f` x2)@). This results
    -- in a thunk chain @O(n)@ elements long, which then must be evaluated from
    -- the outside-in.
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to,
    --
    -- @foldl f z = 'List.foldl' f z . 'toList'@
    --
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    -- There's no point mucking around with coercions here,
    -- because flip forces us to build a new function anyway.

    -- | Left-associative fold of a structure but with strict application of
    -- the operator.
    --
    -- This ensures that each step of the fold is forced to weak head normal
    -- form before being applied, avoiding the collection of thunks that would
    -- otherwise occur. This is often what you want to strictly reduce a finite
    -- list to a single, monolithic result (e.g. 'length').
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to,
    --
    -- @foldl f z = 'List.foldl'' f z . 'toList'@
    --
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x

    -- | A variant of 'foldr' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- @'foldr1' f = 'List.foldr1' f . 'toList'@
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 f xs = fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x m = Just (case m of
                         Nothing -> x
                         Just y  -> f x y)

    -- | A variant of 'foldl' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- @'foldl1' f = 'List.foldl1' f . 'toList'@
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf m y = Just (case m of
                         Nothing -> y
                         Just x  -> f x y)

    -- | List of elements of a structure, from left to right.
    toList :: t a -> [a]
    {-# INLINE toList #-}
    toList t = build (\ c n -> foldr c n t)

    -- | Test whether the structure is empty. The default implementation is
    -- optimized for structures that are similar to cons-lists, because there
    -- is no general way to do better.
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    -- | Returns the size/length of a finite structure as an 'Int'.  The
    -- default implementation is optimized for structures that are similar to
    -- cons-lists, because there is no general way to do better.
    length :: t a -> Int
    length = foldl' (\c _ -> c+1) 0

    -- | Does the element occur in the structure?
    elem :: Eq a => a -> t a -> Bool
    elem = any . (==)

    -- | The largest element of a non-empty structure.
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
       getMax . foldMap (Max #. (Just :: a -> Maybe a))

    -- | The least element of a non-empty structure.
    minimum :: forall a . Ord a => t a -> a
    minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
       getMin . foldMap (Min #. (Just :: a -> Maybe a))

    -- | The 'sum' function computes the sum of the numbers of a structure.
    sum :: Num a => t a -> a
    sum = getSum #. foldMap Sum

    -- | The 'product' function computes the product of the numbers of a
    -- structure.
    product :: Num a => t a -> a
    product = getProduct #. foldMap Product

-- instances for Prelude types

-- | @since 2.01
instance Foldable Maybe where
    foldMap = maybe mempty

    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

    foldl _ z Nothing = z
    foldl f z (Just x) = f z x

-- | @since 2.01
instance Foldable [] where
    elem    = List.elem
    foldl   = List.foldl
    foldl'  = List.foldl'
    foldl1  = List.foldl1
    foldr   = List.foldr
    foldr1  = List.foldr1
    length  = List.length
    maximum = List.maximum
    minimum = List.minimum
    null    = List.null
    product = List.product
    sum     = List.sum
    toList  = id

-- | @since 4.9.0.0
instance Foldable NonEmpty where
  foldr f z ~(a :| as) = f a (List.foldr f z as)
  foldl f z ~(a :| as) = List.foldl f (f z a) as
  foldl1 f ~(a :| as) = List.foldl f a as
  foldMap f ~(a :| as) = f a `mappend` foldMap f as
  fold ~(m :| ms) = m `mappend` fold ms
  length (_ :| as) = 1 + List.length as
  toList ~(a :| as) = a : as

-- | @since 4.7.0.0
instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

    length (Left _)  = 0
    length (Right _) = 1

    null             = isLeft

-- | @since 4.7.0.0
instance Foldable ((,) a) where
    foldMap f (_, y) = f y

    foldr f z (_, y) = f y z

-- | @since 4.8.0.0
instance Foldable (Array i) where
    foldr = foldrElems
    foldl = foldlElems
    foldl' = foldlElems'
    foldr' = foldrElems'
    foldl1 = foldl1Elems
    foldr1 = foldr1Elems
    toList = elems
    length = numElements
    null a = numElements a == 0

-- | @since 4.7.0.0
instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = errorWithoutStackTrace "foldl1: Proxy"
    foldr1 _ _ = errorWithoutStackTrace "foldr1: Proxy"
    length _   = 0
    null _     = True
    elem _ _   = False
    sum _      = 0
    product _  = 1

-- | @since 4.8.0.0
instance Foldable Dual where
    foldMap            = coerce

    elem               = (. getDual) #. (==)
    foldl              = coerce
    foldl'             = coerce
    foldl1 _           = getDual
    foldr f z (Dual x) = f x z
    foldr'             = foldr
    foldr1 _           = getDual
    length _           = 1
    maximum            = getDual
    minimum            = getDual
    null _             = False
    product            = getDual
    sum                = getDual
    toList (Dual x)    = [x]

-- | @since 4.8.0.0
instance Foldable Sum where
    foldMap            = coerce

    elem               = (. getSum) #. (==)
    foldl              = coerce
    foldl'             = coerce
    foldl1 _           = getSum
    foldr f z (Sum x)  = f x z
    foldr'             = foldr
    foldr1 _           = getSum
    length _           = 1
    maximum            = getSum
    minimum            = getSum
    null _             = False
    product            = getSum
    sum                = getSum
    toList (Sum x)     = [x]

-- | @since 4.8.0.0
instance Foldable Product where
    foldMap               = coerce

    elem                  = (. getProduct) #. (==)
    foldl                 = coerce
    foldl'                = coerce
    foldl1 _              = getProduct
    foldr f z (Product x) = f x z
    foldr'                = foldr
    foldr1 _              = getProduct
    length _              = 1
    maximum               = getProduct
    minimum               = getProduct
    null _                = False
    product               = getProduct
    sum                   = getProduct
    toList (Product x)    = [x]

-- | @since 4.8.0.0
instance Foldable First where
    foldMap f = foldMap f . getFirst

-- | @since 4.8.0.0
instance Foldable Last where
    foldMap f = foldMap f . getLast

-- Instances for GHC.Generics
-- | @since 4.9.0.0
instance Foldable U1 where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = errorWithoutStackTrace "foldl1: U1"
    foldr1 _ _ = errorWithoutStackTrace "foldr1: U1"
    length _   = 0
    null _     = True
    elem _ _   = False
    sum _      = 0
    product _  = 1

deriving instance Foldable V1
deriving instance Foldable Par1
deriving instance Foldable f => Foldable (Rec1 f)
deriving instance Foldable (K1 i c)
deriving instance Foldable f => Foldable (M1 i c f)
deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)
deriving instance (Foldable f, Foldable g) => Foldable (f :*: g)
deriving instance (Foldable f, Foldable g) => Foldable (f :.: g)
deriving instance Foldable UAddr
deriving instance Foldable UChar
deriving instance Foldable UDouble
deriving instance Foldable UFloat
deriving instance Foldable UInt
deriving instance Foldable UWord

-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl f' return xs z0
  where f' k x z = f x z >>= k

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k

-- | Map each element of a structure to an action, evaluate these
-- actions from left to right, and ignore the results. For a version
-- that doesn't ignore the results see 'Data.Traversable.traverse'.
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

-- | 'for_' is 'traverse_' with its arguments flipped. For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.
--
-- >>> for_ [1..4] print
-- 1
-- 2
-- 3
-- 4
for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
{-# INLINE for_ #-}
for_ = flip traverse_

-- | Map each element of a structure to a monadic action, evaluate
-- these actions from left to right, and ignore the results. For a
-- version that doesn't ignore the results see
-- 'Data.Traversable.mapM'.
--
-- As of base 4.8.0.0, 'mapM_' is just 'traverse_', specialized to
-- 'Monad'.
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f= foldr ((>>) . f) (return ())

-- | 'forM_' is 'mapM_' with its arguments flipped. For a version that
-- doesn't ignore the results see 'Data.Traversable.forM'.
--
-- As of base 4.8.0.0, 'forM_' is just 'for_', specialized to 'Monad'.
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = flip mapM_

-- | Evaluate each action in the structure from left to right, and
-- ignore the results. For a version that doesn't ignore the results
-- see 'Data.Traversable.sequenceA'.
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

-- | Evaluate each monadic action in the structure from left to right,
-- and ignore the results. For a version that doesn't ignore the
-- results see 'Data.Traversable.sequence'.
--
-- As of base 4.8.0.0, 'sequence_' is just 'sequenceA_', specialized
-- to 'Monad'.
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- asum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"
asum :: (Foldable t, Alternative f) => t (f a) -> f a
{-# INLINE asum #-}
asum = foldr (<|>) empty

-- | The sum of a collection of actions, generalizing 'concat'.
-- As of base 4.8.0.0, 'msum' is just 'asum', specialized to 'MonadPlus'.
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
{-# INLINE msum #-}
msum = asum

-- | The concatenation of all the elements of a container of lists.
concat :: Foldable t => t [a] -> [a]
concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
{-# INLINE concat #-}

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists.
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
{-# INLINE concatMap #-}

-- These use foldr rather than foldMap to avoid repeated concatenation.

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
and :: Foldable t => t Bool -> Bool
and = getAll #. foldMap All

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
or :: Foldable t => t Bool -> Bool
or = getAny #. foldMap Any

-- | Determines whether any element of the structure satisfies the predicate.
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny #. foldMap (Any #. p)

-- | Determines whether all elements of the structure satisfy the predicate.
all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll #. foldMap (All #. p)

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-- | 'notElem' is the negation of 'elem'.
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = not . elem x

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))

{-
Note [maximumBy/minimumBy space usage]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the type signatures of maximumBy and minimumBy were generalized to work
over any Foldable instance (instead of just lists), they were defined using
foldr1. This was problematic for space usage, as the semantics of maximumBy
and minimumBy essentially require that they examine every element of the
data structure. Using foldr1 to examine every element results in space usage
proportional to the size of the data structure. For the common case of lists,
this could be particularly bad (see Trac #10830).

For the common case of lists, switching the implementations of maximumBy and
minimumBy to foldl1 solves the issue, as GHC's strictness analysis can then
make these functions only use O(1) stack space. It is perhaps not the optimal
way to fix this problem, as there are other conceivable data structures
(besides lists) which might benefit from specialized implementations for
maximumBy and minimumBy (see
https://ghc.haskell.org/trac/ghc/ticket/10830#comment:26 for a further
discussion). But using foldl1 is at least always better than using foldr1, so
GHC has chosen to adopt that approach for now.
-}
