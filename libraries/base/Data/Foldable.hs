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

    -- * Overview
    -- $overview

    -- ** Recursive and corecursive reduction
    -- $reduction

    -- *** Strict recursive folds
    -- $strict

    -- **** List of strict functions
    -- $strictlist

    -- *** Lazy corecursive folds
    -- $lazy

    -- **** List of lazy functions
    -- $lazylist

    -- *** Short-circuit folds
    -- $shortcircuit

    -- **** List of short-circuit functions
    -- $shortlist

    -- *** Hybrid folds
    -- $hybrid

    -- ** Avoiding multi-pass algorithms
    -- $multipass

    -- * Defining instances
    -- $instances

    -- *** Being strict by being lazy
    -- $strictlazy

    -- * Laws
    -- $laws

    -- * Notes
    -- $notes

    -- * See also
    -- $also
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

-- XXX: Missing haddock feature.  Links to anchors in other modules
-- don't have a sensible way to name the link within the module itself.
-- Thus, the below "Data.Foldable#overview" works well when shown as
-- @Data.Foldable@ from other modules, but in the home module it should
-- be possible to specify alternative link text. :-(

-- | The Foldable class represents data structures that can be reduced to a
-- summary value one element at a time.  Strict left-associative folds are a
-- good fit for space-efficient reduction, while lazy right-associative folds
-- are good fit for corecursive iteration or for folds that short-circuit after
-- processing an initial subsequence of the structure's elements.
--
-- A more detailed description can be found in the overview section of
-- "Data.Foldable#overview".
--
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Combine the elements of a structure using a monoid.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> fold [[1, 2, 3], [4, 5], [6], []]
    -- [1,2,3,4,5,6]
    --
    -- >>> fold [Sum 1, Sum 3, Sum 5]
    -- Sum {getSum = 9}
    --
    -- Infinite structures never terminate:
    --
    -- >>> fold (repeat Nothing)
    -- * Hangs forever *
    fold :: Monoid m => t m -> m
    fold = foldMap id

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> foldMap Sum [1, 3, 5]
    -- Sum {getSum = 9}
    --
    -- >>> foldMap Product [1, 3, 5]
    -- Product {getProduct = 15}
    --
    -- >>> foldMap (replicate 3) [1, 2, 3]
    -- [1,1,1,2,2,2,3,3,3]
    --
    -- Infinite structures never terminate:
    --
    -- >>> foldMap Sum [1..]
    -- * Hangs forever *
    foldMap :: Monoid m => (a -> m) -> t a -> m
    {-# INLINE foldMap #-}
    -- This INLINE allows more list functions to fuse. See #9848.
    foldMap f = foldr (mappend . f) mempty

    -- | A variant of 'foldMap' that is strict in the accumulator.
    --
    -- @since 4.13.0.0
    foldMap' :: Monoid m => (a -> m) -> t a -> m
    foldMap' f = foldl' (\ acc a -> acc <> f a) mempty

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
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> foldr (||) False [False, True, False]
    -- True
    --
    -- >>> foldr (||) False []
    -- False
    --
    -- >>> foldr (\nextChar reversedString -> reversedString ++ [nextChar]) "foo" ['a', 'b', 'c', 'd']
    -- "foodcba"
    --
    -- ===== Infinite structures
    --
    -- ⚠️ Applying 'foldr' to infinite structures usually doesn't terminate.
    --
    -- It may still terminate in one of the following conditions:
    --
    -- * the folding function is short-circuiting
    -- * the folding function is lazy on its second argument
    --
    -- ====== Short-circuiting
    --
    -- '(||)' short-circuits on 'True' values, so the following terminates because there is a 'True' value finitely far from the left side:
    --
    -- >>> foldr (||) False (True : repeat False)
    -- True
    --
    -- But the following doesn't terminate:
    --
    -- >>> foldr (||) False (repeat False ++ [True])
    -- * Hangs forever *
    --
    -- ====== Laziness in the second argument
    --
    -- Applying 'foldr' to infinite structures terminates when the folding function is lazy on its second argument:
    --
    -- >>> foldr (\nextElement accumulator -> nextElement : fmap (+3) accumulator) [42] (repeat 1)
    -- [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43...
    --
    -- >>> take 5 $ foldr (\nextElement accumulator -> nextElement : fmap (+3) accumulator) [42] (repeat 1)
    -- [1,4,7,10,13]
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    -- | Right-associative fold of a structure, but with strict application of
    -- the operator.
    --
    -- @since 4.6.0.0
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
    -- entire input list must be traversed. This means that 'foldl' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use `foldl'` instead of 'foldl'. The reason for this is that latter does
    -- not force the "inner" results (e.g. @z \`f\` x1@ in the above example)
    -- before applying them to the operator (e.g. to @(\`f\` x2)@). This results
    -- in a thunk chain \(\mathcal{O}(n)\) elements long, which then must be
    -- evaluated from the outside-in.
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to,
    --
    -- @foldl f z = 'List.foldl' f z . 'toList'@
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> foldl (+) 42 (Node (Leaf 1) 3 (Node Empty 4 (Leaf 2)))
    -- 52
    --
    -- >>> foldl (+) 42 Empty
    -- 42
    --
    -- >>> foldl (\string nextElement -> nextElement : string) "abcd" (Node (Leaf 'd') 'e' (Node Empty 'f' (Leaf 'g')))
    -- "gfedabcd"
    --
    -- Left-folding infinite structures never terminates:
    --
    -- >>> let infiniteNode = Node Empty 1 infiniteNode in foldl (+) 42 infiniteNode
    -- * Hangs forever *
    --
    -- Evaluating the head of the result (when applicable) does not terminate, unlike 'foldr':
    --
    -- >>> let infiniteNode = Node Empty 'd' infiniteNode in take 5 (foldl (\string nextElement -> nextElement : string) "abc" infiniteNode)
    -- * Hangs forever *
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
    -- @foldl' f z = 'List.foldl'' f z . 'toList'@
    --
    -- @since 4.6.0.0
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x

    -- | A variant of 'foldr' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- ⚠️ This function is non-total and will raise a runtime exception if the structure happens to be empty.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> foldr1 (+) [1..4]
    -- 10
    --
    -- >>> foldr1 (+) []
    -- Exception: Prelude.foldr1: empty list
    --
    -- >>> foldr1 (+) Nothing
    -- *** Exception: foldr1: empty structure
    --
    -- >>> foldr1 (-) [1..4]
    -- -2
    --
    -- >>> foldr1 (&&) [True, False, True, True]
    -- False
    --
    -- >>> foldr1 (||) [False, False, True, True]
    -- True
    --
    -- >>> foldr1 (+) [1..]
    -- * Hangs forever *
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
    -- ⚠️ This function is non-total and will raise a runtime exception if the structure happens to be empty.
    --
    -- @'foldl1' f = 'List.foldl1' f . 'toList'@
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> foldl1 (+) [1..4]
    -- 10
    --
    -- >>> foldl1 (+) []
    -- *** Exception: Prelude.foldl1: empty list
    --
    -- >>> foldl1 (+) Nothing
    -- *** Exception: foldl1: empty structure
    --
    -- >>> foldl1 (-) [1..4]
    -- -8
    --
    -- >>> foldl1 (&&) [True, False, True, True]
    -- False
    --
    -- >>> foldl1 (||) [False, False, True, True]
    -- True
    --
    -- >>> foldl1 (+) [1..]
    -- * Hangs forever *
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf m y = Just (case m of
                         Nothing -> y
                         Just x  -> f x y)

    -- | List of elements of a structure, from left to right.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> toList Nothing
    -- []
    --
    -- >>> toList (Just 42)
    -- [42]
    --
    -- >>> toList (Left "foo")
    -- []
    --
    -- >>> toList (Node (Leaf 5) 17 (Node Empty 12 (Leaf 8)))
    -- [5,17,12,8]
    --
    -- For lists, 'toList' is the identity:
    --
    -- >>> toList [1, 2, 3]
    -- [1,2,3]
    --
    -- @since 4.8.0.0
    toList :: t a -> [a]
    {-# INLINE toList #-}
    toList t = build (\ c n -> foldr c n t)

    -- | Test whether the structure is empty. The default implementation is
    -- optimized for structures that are similar to cons-lists, because there
    -- is no general way to do better.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> null []
    -- True
    --
    -- >>> null [1]
    -- False
    --
    -- 'null' terminates even for infinite structures:
    --
    -- >>> null [1..]
    -- False
    --
    -- @since 4.8.0.0
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    -- | Returns the size/length of a finite structure as an 'Int'.  The
    -- default implementation is optimized for structures that are similar to
    -- cons-lists, because there is no general way to do better.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> length []
    -- 0
    --
    -- >>> length ['a', 'b', 'c']
    -- 3
    -- >>> length [1..]
    -- * Hangs forever *
    --
    -- @since 4.8.0.0
    length :: t a -> Int
    length = foldl' (\c _ -> c+1) 0

    -- | Does the element occur in the structure?
    --
    -- Note: 'elem' is often used in infix form.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> 3 `elem` []
    -- False
    --
    -- >>> 3 `elem` [1,2]
    -- False
    --
    -- >>> 3 `elem` [1,2,3,4,5]
    -- True
    --
    -- For infinite structures, 'elem' terminates if the value exists at a
    -- finite distance from the left side of the structure:
    --
    -- >>> 3 `elem` [1..]
    -- True
    --
    -- >>> 3 `elem` ([4..] ++ [3])
    -- * Hangs forever *
    --
    -- @since 4.8.0.0
    elem :: Eq a => a -> t a -> Bool
    elem = any . (==)

    -- | The largest element of a non-empty structure.
    --
    -- ⚠️ This function is non-total and will raise a runtime exception if the structure happens to be empty.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> maximum [1..10]
    -- 10
    --
    -- >>> maximum []
    -- *** Exception: Prelude.maximum: empty list
    --
    -- >>> maximum Nothing
    -- *** Exception: maximum: empty structure
    --
    -- @since 4.8.0.0
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
       getMax . foldMap' (Max #. (Just :: a -> Maybe a))
    {-# INLINEABLE maximum #-}

    -- | The least element of a non-empty structure.
    --
    -- ⚠️ This function is non-total and will raise a runtime exception if the structure happens to be empty
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> minimum [1..10]
    -- 1
    --
    -- >>> minimum []
    -- *** Exception: Prelude.minimum: empty list
    --
    -- >>> minimum Nothing
    -- *** Exception: minimum: empty structure
    --
    -- @since 4.8.0.0
    minimum :: forall a . Ord a => t a -> a
    minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
       getMin . foldMap' (Min #. (Just :: a -> Maybe a))
    {-# INLINEABLE minimum #-}

    -- | The 'sum' function computes the sum of the numbers of a structure.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> sum []
    -- 0
    --
    -- >>> sum [42]
    -- 42
    --
    -- >>> sum [1..10]
    -- 55
    --
    -- >>> sum [4.1, 2.0, 1.7]
    -- 7.8
    --
    -- >>> sum [1..]
    -- * Hangs forever *
    --
    -- @since 4.8.0.0
    sum :: Num a => t a -> a
    sum = getSum #. foldMap' Sum
    {-# INLINEABLE sum #-}

    -- | The 'product' function computes the product of the numbers of a
    -- structure.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> product []
    -- 1
    --
    -- >>> product [42]
    -- 42
    --
    -- >>> product [1..10]
    -- 3628800
    --
    -- >>> product [4.1, 2.0, 1.7]
    -- 13.939999999999998
    --
    -- >>> product [1..]
    -- * Hangs forever *
    --
    -- @since 4.8.0.0
    product :: Num a => t a -> a
    product = getProduct #. foldMap' Product
    {-# INLINEABLE product #-}

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
  foldl f z (a :| as) = List.foldl f (f z a) as
  foldl1 f (a :| as) = List.foldl f a as

  -- GHC isn't clever enough to transform the default definition
  -- into anything like this, so we'd end up shuffling a bunch of
  -- Maybes around.
  foldr1 f (p :| ps) = foldr go id ps p
    where
      go x r prev = f prev (r x)

  -- We used to say
  --
  --   length (_ :| as) = 1 + length as
  --
  -- but the default definition is better, counting from 1.
  --
  -- The default definition also works great for null and foldl'.
  -- As usual for cons lists, foldr' is basically hopeless.

  foldMap f ~(a :| as) = f a `mappend` foldMap f as
  fold ~(m :| ms) = m `mappend` fold ms
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
    length _  = 1
    null _ = False

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

-- | @since 4.12.0.0
instance (Foldable f) => Foldable (Alt f) where
    foldMap f = foldMap f . getAlt

-- | @since 4.12.0.0
instance (Foldable f) => Foldable (Ap f) where
    foldMap f = foldMap f . getAp

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

-- | @since 4.9.0.0
deriving instance Foldable V1

-- | @since 4.9.0.0
deriving instance Foldable Par1

-- | @since 4.9.0.0
deriving instance Foldable f => Foldable (Rec1 f)

-- | @since 4.9.0.0
deriving instance Foldable (K1 i c)

-- | @since 4.9.0.0
deriving instance Foldable f => Foldable (M1 i c f)

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :*: g)

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :.: g)

-- | @since 4.9.0.0
deriving instance Foldable UAddr

-- | @since 4.9.0.0
deriving instance Foldable UChar

-- | @since 4.9.0.0
deriving instance Foldable UDouble

-- | @since 4.9.0.0
deriving instance Foldable UFloat

-- | @since 4.9.0.0
deriving instance Foldable UInt

-- | @since 4.9.0.0
deriving instance Foldable UWord

-- Instances for Data.Ord
-- | @since 4.12.0.0
deriving instance Foldable Down

-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> foldrM (\string acc -> print string >> pure (acc + length string)) 42 ["Hello", "world", "!"]
-- "!"
-- "world"
-- "Hello"
-- 53
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c k x z = f x z >>= k
        {-# INLINE c #-}

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> foldlM (\acc string -> print string >> pure (acc + length string)) 42 ["Hello", "world", "!"]
-- "Hello"
-- "world"
-- "!"
-- 53
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c x k z = f z x >>= k
        {-# INLINE c #-}

-- | Map each element of a structure to an action, evaluate these
-- actions from left to right, and ignore the results. For a version
-- that doesn't ignore the results see 'Data.Traversable.traverse'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> traverse_ print ["Hello", "world", "!"]
-- "Hello"
-- "world"
-- "!"
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr c (pure ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x *> k
        {-# INLINE c #-}

-- | 'for_' is 'traverse_' with its arguments flipped. For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.
--
-- ==== __Examples__
--
-- Basic usage:
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
mapM_ f = foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x >> k
        {-# INLINE c #-}

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
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> sequenceA_ [print "Hello", print "world", print "!"]
-- "Hello"
-- "world"
-- "!"
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr c (pure ())
  -- See Note [List fusion and continuations in 'c']
  where c m k = m *> k
        {-# INLINE c #-}

-- | Evaluate each monadic action in the structure from left to right,
-- and ignore the results. For a version that doesn't ignore the
-- results see 'Data.Traversable.sequence'.
--
-- As of base 4.8.0.0, 'sequence_' is just 'sequenceA_', specialized
-- to 'Monad'.
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c m k = m >> k
        {-# INLINE c #-}

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> asum [Just "Hello", Nothing, Just "World"]
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
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> concat (Just [1, 2, 3])
-- [1,2,3]
--
-- >>> concat (Node (Leaf [1, 2, 3]) [4, 5] (Node Empty [6] (Leaf [])))
-- [1,2,3,4,5,6]
concat :: Foldable t => t [a] -> [a]
concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
{-# INLINE concat #-}

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> concatMap (take 3) [[1..], [10..], [100..], [1000..]]
-- [1,2,3,10,11,12,100,101,102,1000,1001,1002]
--
-- >>> concatMap (take 3) (Node (Leaf [1..]) [10..] Empty)
-- [1,2,3,10,11,12]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
{-# INLINE concatMap #-}

-- These use foldr rather than foldMap to avoid repeated concatenation.

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> and []
-- True
--
-- >>> and [True]
-- True
--
-- >>> and [False]
-- False
--
-- >>> and [True, True, False]
-- False
--
-- >>> and (False : repeat True) -- Infinite list [False,True,True,True,True,True,True...
-- False
--
-- >>> and (repeat True)
-- * Hangs forever *
and :: Foldable t => t Bool -> Bool
and = getAll #. foldMap All

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> or []
-- False
--
-- >>> or [True]
-- True
--
-- >>> or [False]
-- False
--
-- >>> or [True, True, False]
-- True
--
-- >>> or (True : repeat False) -- Infinite list [True,False,False,False,False,False,False...
-- True
--
-- >>> or (repeat False)
-- * Hangs forever *
or :: Foldable t => t Bool -> Bool
or = getAny #. foldMap Any

-- | Determines whether any element of the structure satisfies the predicate.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> any (> 3) []
-- False
--
-- >>> any (> 3) [1,2]
-- False
--
-- >>> any (> 3) [1,2,3,4,5]
-- True
--
-- >>> any (> 3) [1..]
-- True
--
-- >>> any (> 3) [0, -1..]
-- * Hangs forever *
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny #. foldMap (Any #. p)

-- | Determines whether all elements of the structure satisfy the predicate.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> all (> 3) []
-- True
--
-- >>> all (> 3) [1,2]
-- False
--
-- >>> all (> 3) [1,2,3,4,5]
-- False
--
-- >>> all (> 3) [1..]
-- False
--
-- >>> all (> 3) [4..]
-- * Hangs forever *
all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll #. foldMap (All #. p)

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> maximumBy (compare `on` length) ["Hello", "World", "!", "Longest", "bar"]
-- "Longest"

-- See Note [maximumBy/minimumBy space usage]
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = fromMaybe (errorWithoutStackTrace "maximumBy: empty structure")
  . foldl' max' Nothing
  where
    max' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> x
        _ -> y
{-# INLINEABLE maximumBy #-}

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> minimumBy (compare `on` length) ["Hello", "World", "!", "Longest", "bar"]
-- "!"

-- See Note [maximumBy/minimumBy space usage]
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = fromMaybe (errorWithoutStackTrace "minimumBy: empty structure")
  . foldl' min' Nothing
  where
    min' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> y
        _ -> x
{-# INLINEABLE minimumBy #-}

-- | 'notElem' is the negation of 'elem'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> 3 `notElem` []
-- True
--
-- >>> 3 `notElem` [1,2]
-- True
--
-- >>> 3 `notElem` [1,2,3,4,5]
-- False
--
-- For infinite structures, 'notElem' terminates if the value exists at a
-- finite distance from the left side of the structure:
--
-- >>> 3 `notElem` [1..]
-- False
--
-- >>> 3 `notElem` ([4..] ++ [3])
-- * Hangs forever *
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = not . elem x

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> find (> 42) [0, 5..]
-- Just 45
--
-- >>> find (> 4) (Node (Leaf 3) 17 (Node Empty 12 (Leaf 8)))
-- Just 17
--
-- >>> find (> 12) [1..7]
-- Nothing
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))

{-
Note [List fusion and continuations in 'c']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we define
  mapM_ f = foldr ((>>) . f) (return ())
(this is the way it used to be).

Now suppose we want to optimise the call

  mapM_ <big> (build g)
    where
  g c n = ...(c x1 y1)...(c x2 y2)....n...

GHC used to proceed like this:

  mapM_ <big> (build g)

  = { Definition of mapM_ }
    foldr ((>>) . <big>) (return ()) (build g)

  = { foldr/build rule }
    g ((>>) . <big>) (return ())

  = { Inline g }
    let c = (>>) . <big>
        n = return ()
    in ...(c x1 y1)...(c x2 y2)....n...

The trouble is that `c`, being big, will not be inlined.  And that can
be absolutely terrible for performance, as we saw in #8763.

It's much better to define

  mapM_ f = foldr c (return ())
    where
      c x k = f x >> k
      {-# INLINE c #-}

Now we get
  mapM_ <big> (build g)

  = { inline mapM_ }
    foldr c (return ()) (build g)
      where c x k = f x >> k
            {-# INLINE c #-}
            f = <big>

Notice that `f` does not inline into the RHS of `c`,
because the INLINE pragma stops it; see
Note [Simplifying inside stable unfoldings] in GHC.Core.Opt.Simplify.Utils.
Continuing:

  = { foldr/build rule }
    g c (return ())
      where ...
         c x k = f x >> k
         {-# INLINE c #-}
            f = <big>

  = { inline g }
    ...(c x1 y1)...(c x2 y2)....n...
      where c x k = f x >> k
            {-# INLINE c #-}
            f = <big>
            n = return ()

      Now, crucially, `c` does inline

  = { inline c }
    ...(f x1 >> y1)...(f x2 >> y2)....n...
      where f = <big>
            n = return ()

And all is well!  The key thing is that the fragment
`(f x1 >> y1)` is inlined into the body of the builder
`g`.
-}

{-
Note [maximumBy/minimumBy space usage]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the type signatures of maximumBy and minimumBy were generalized to work
over any Foldable instance (instead of just lists), they were defined using
foldr1. This was problematic for space usage, as the semantics of maximumBy
and minimumBy essentially require that they examine every element of the
data structure. Using foldr1 to examine every element results in space usage
proportional to the size of the data structure. For the common case of lists,
this could be particularly bad (see #10830).

For the common case of lists, switching the implementations of maximumBy and
minimumBy to foldl1 solves the issue, assuming GHC's strictness analysis can then
make these functions only use O(1) stack space. As of base 4.16, we have
switched to employing foldl' over foldl1, not relying on GHC's optimiser. See
https://gitlab.haskell.org/ghc/ghc/-/issues/17867 for more context.
-}

--------------

-- In order to avoid having actual Unicode glyphs in the module source,
-- the below numeric HTML entity codes are used:
--
-- * ellipsis = &#x2026;

-- $overview
--
-- #overview#
-- Foldable structures are reduced to a summary value by accumulating
-- contributions to the result one element at a time.
--
-- == Left and right folds
--
-- #leftright#
-- Merging the contribution of the current element with an accumulator value
-- from a partial result is performed by an /update function/, either
-- explicitly provided by the caller as in `foldr`, implicit as in `length`, or
-- partly implicit as in `foldMap` (where each element is mapped into a monoid,
-- and the Monoid's `mappend` performs the merge).
--
-- A key distinction is between left-associative and right-associative
-- folds:
--
-- * In left-associative folds the accumulator is a partial fold over the
--   elements that __precede__ the current element, and is passed to the update
--   function as its first (left) argument.  The outer-most application of the
--   update function merges the contribution of the last element of the
--   structure with the contributions of all its predecessors.
--
-- * In right-associative folds the accumulator is a partial fold over the
--   elements that __follow__ the current element, and is passed to the update
--   function as its second (right) argument.  The outer-most application of
--   the update function merges the contribution of the first element of the
--   structure with the contributions of all its successors.
--
-- These two types of folds are typified by the left-associative strict
-- `foldl'` and the right-associative lazy `foldr`.
--
-- @
-- `foldl'` :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- `foldr`  :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- @
--
-- Example usage:
--
-- >>> foldl' (+) 0 [1..100]
-- 5050
-- >>> foldr (&&) True (repeat False)
-- False
--
-- The first argument of both is an explicit /update function/ that merges the
-- contribution of an element of the structure with a partial fold over,
-- respectively, either the preceding or following elements of the structure.
--
-- The second argument of both is an initial accumulator value @z@ of type
-- @b@.  This is the result of the fold when the structure is empty.
-- When the structure is non-empty, this is the accumulator value merged with
-- the first element in left-associative folds, or with the last element in
-- right-associative folds.
--
-- The third and final argument is a @Foldable@ structure containing elements
-- @(a, b, c, &#x2026;)@.
--
-- * __`foldl'`__ takes an update function of the form:
--
--     @
--     f :: b -- accumulated fold of the initial elements
--       -> a -- current element
--       -> b -- updated fold, inclusive of current element
--     @
--
--     If the structure's last element is @y@, the result of the fold is:
--
--     @
--     g y . &#x2026; . g c . g b . g a $ z
--       where g element !acc = f acc element
--     @
--
--     Since `foldl'` is strict in the accumulator, this is always
--     a [strict](#strict) reduction with no opportunity for early return or
--     intermediate results.  The structure must be finite, since no result is
--     returned until the last element is processed.  The advantage of
--     strictness is space efficiency: the final result can be computed without
--     storing a potentially deep stack of lazy intermediate results.
--
-- * __`foldr`__ takes an update function of the form:
--
--     @
--     f :: a -- current element
--       -> b -- accumulated fold of the remaining elements
--       -> b -- updated fold, inclusive of current element
--     @
--
--     the result of the fold is:
--
--     @
--     f a . f b . f c . &#x2026; $ z
--     @
--
--     If each call of @f@ on the current element @e@, (referenced as @(f e)@
--     below) returns a structure in which its second argument is captured in a
--     lazily-evaluated component, then the fold of the remaining elements is
--     available to the caller of `foldr` as a pending computation (thunk) that
--     is computed only when that component is evaluated.
--
--     Alternatively, if any of the @(f e)@ ignore their second argument, the
--     fold stops there, with the remaining elements unused.  As a result,
--     `foldr` is well suited to define both [corecursive](#corec)
--     and [short-circuit](#short) reductions.
--
--     When the update function is always strict in the second argument,
--     `foldl'` is generally a better choice than `foldr`.  When `foldr` is
--     called with a strict update function, evaluation cannot begin until the
--     last element is reached, by which point a deep stack of pending function
--     applications may have been built up in memory.
--
-- In finite structures for which right-to-left sequencing no less efficient as
-- left-to-right sequencing, there is no inherent performance distinction
-- between left-associative and right-associative folds.  If the structure's
-- @Foldable@ instance takes advantage of this symmetry to also make strict
-- right folds space-efficient and lazy left folds corecursive, one need only
-- take care to choose either a strict or lazy method for the task at hand.

--------------

-- $reduction
--
-- As observed in the [above description](#leftright) of left and right folds,
-- there are three general ways in which a structure can be reduced to a
-- summary value:
--
-- * __Recursive__ reduction, which is strict in all the elements of the
--   structure.  This produces a single final result only after processing the
--   entire input structure, and so the input must be finite.
--
-- * __Corecursion__, which yields intermediate results as it encounters
--   additional input elements. Lazy processing of the remaining elements
--   makes the intermediate results available even before the rest of the
--   input is processed.  The input may be unbounded, and the caller can
--   stop processing intermediate results early.
--
-- * __Short-circuit__ reduction, which examines some initial sequence of the
--   input elements, but stops once a termination condition is met, returning a
--   final result based only on the elements considered up to that point.  The
--   remaining elements are not considered.  The input should generally be
--   finite, because the termination condition might otherwise never be met.
--
-- Whether a fold is recursive, corecursive or short-circuiting can depend on
-- both the method chosen to perform the fold and on the update function passed
-- to that method (which may be implicit, as with the `mappend` method of a
-- monoid instance).
--
-- There are also hybrid cases, where the method and/or the update function are
-- not well suited to the task at hand, resulting in a fold that fails to yield
-- incremental results until the entire input is processed, or fails to
-- strictly evaluate results as it goes, deferring all the work to the
-- evaluation of a large final thunk. Such cases should be avoided, either by
-- selecting a more appropriate @Foldable@ method, or by tailoring the update
-- function to the chosen method.
--
-- The distinction between these types of folds is critical, both in deciding
-- which @Foldable@ method to use to perform the reduction efficiently, and in
-- writing @Foldable@ instances for new structures.  Below is a more detailed
-- overview of each type.

--------------

-- $strict
-- #strict#
--
-- Common examples of strict recursive reduction are the various /aggregate/
-- functions, like 'sum', 'product', 'length', as well as more complex
-- summaries such as frequency counts.  These functions return only a single
-- value after processing the entire input structure.  In such cases, lazy
-- processing of the tail of the input structure is generally not only
-- unnecessary, but also inefficient.  Thus, these and similar folds should be
-- implemented in terms of strict left-associative @Foldable@ methods (typically
-- `foldl'`) to perform an efficient reduction in constant space.
--
-- Conversely, an implementation of @Foldable@ for a new structure should
-- ensure that `foldl'` actually performs a strict left-associative reduction.
--
-- The `foldMap'` method is a special case of `foldl'`, in which the initial
-- accumulator is `mempty` and the update function is @mappend . f@, where
-- @f@ maps each input element into the 'Monoid' in question.  Therefore,
-- `foldMap'` is an appropriate choice under essentially the same conditions
-- as `foldl'`, and its implementation for a given @Foldable@ structure should
-- also be a strict left-associative reduction.
--
-- While the examples below are not necessarily the most optimal definitions of
-- the intended functions, they are all cases in which `foldMap'` is far more
-- appropriate (as well as more efficient) than the lazy `foldMap`.
--
-- > length  = getSum     . foldMap' (const (Sum 1))
-- > sum     = getSum     . foldMap' Sum
-- > product = getProduct . foldMap' Product
--
-- [ The actual default definitions employ coercions to optimise out
--   'getSum' and 'getProduct'. ]

--------------

-- $strictlist
--
-- The full list of strict recursive functions in this module is:
--
-- * Provided the update function is strict in its left argument:
--
--     @
--     `foldl'` :: Foldable t => (b -> a -> b) -> b -> t a -> b
--     @
--
-- * Provided `mappend` is strict in its left argument:
--
--     @
--     `foldMap'` :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--     @
--
-- * Provided the instance is correctly defined:
--
--     @
--     `length`    :: Foldable t => t a -> Int
--     `sum`       :: (Foldable t, Num a) => t a -> a
--     `product`   :: (Foldable t, Num a) => t a -> a
--     `maximum`   :: (Foldable t, Ord a) => t a -> a
--     `minimum`   :: (Foldable t, Ord a) => t a -> a
--     `maximumBy` :: Foldable t => (a -> a -> Ordering) -> t a -> a
--     `minimumBy` :: Foldable t => (a -> a -> Ordering) -> t a -> a
--     @

--------------

-- $lazy
--
-- #corec#
-- Common examples of lazy corecursive reduction are functions that map and
-- flatten a structure to a lazy stream of result values, i.e.  an iterator
-- over the transformed input elements. In such cases, it is important to
-- choose a @Foldable@ method that is lazy in the tail of the structure, such
-- as `foldr` (or `foldMap`, if the result @Monoid@ has a lazy `mappend` as
-- with e.g. ByteString Builders).
--
-- Conversely, an implementation of `foldr` for a structure that can
-- accommodate a large (and possibly unbounded) number of elements is expected
-- to be lazy in the tail of the input, allowing update functions that are lazy
-- in the accumulator to yield intermediate results incrementally.  Such folds
-- are right-associative, with the tail of the stream returned as a lazily
-- evaluated component of the result (an element of a tuple or some other
-- non-strict constructor, e.g. the @(:)@ constructor for lists).
--
-- The @toList@ function below lazily transforms a @Foldable@ structure to a
-- List.  Note that this transformation may be lossy, e.g.  for a keyed
-- container (@Map@, @HashMap@, &#x2026;) the output stream holds only the
-- values, not the keys.  Lossless transformations to\/from lists of @(key,
-- value)@ pairs are typically available in the modules for the specific
-- container types.
--
-- > toList = foldr (:) []

--------------

-- $lazylist
--
-- The full list of lazy corecursive functions in this module is:
--
-- * Provided the reduction function is lazy in its second argument,
--   (otherwise best to use a strict recursive reduction):
--
--     @
--     `foldr`  :: Foldable t => (a -> b -> b) -> b -> t a -> b
--     `foldr1` :: Foldable t => (a -> a -> a) -> t a -> a
--     @
--
-- * Provided the 'Monoid' `mappend` is lazy in its second argument
--   (otherwise best to use a strict recursive reduction):
--
--     @
--     `fold`    :: Foldable t => Monoid m => t m -> m
--     `foldMap` :: Foldable t => Monoid m => (a -> m) -> t a -> m
--     @
--
-- * Provided the instance is correctly defined:
--
--     @
--     `toList`    :: Foldable t => t a -> [a]
--     `concat`    :: Foldable t => t [a] -> [a]
--     `concatMap` :: Foldable t => (a -> [b]) -> t a -> [b]
--     @

--------------

-- $shortcircuit
--
-- #short#
-- Examples of short-cicuit reduction include various boolean predicates
-- that test whether some or all the elements of a structure satisfy a
-- given condition.  Because these don't necessarily consume the entire
-- list, they typically employ `foldr` with an update function that
-- is conditionally strict in its second argument.  Once the termination
-- condition is met the second argument (tail of the input structure) is
-- ignored.  No result is returned until that happens.
--
-- The key distinguishing feature of these folds is /conditional/ strictness
-- in the second argument, it is sometimes evaluated and sometimes not.
--
-- The simplest (degenerate case) of these is 'null', which determines whether
-- a structure is empty or not.  This only needs to look at the first element,
-- and only to the extent of whether it exists or not, and not its value.  In
-- this case termination is guaranteed, and infinite input structures are fine.
-- Its default definition is of course in terms of the lazy 'foldr':
--
-- > null = foldr (\_ _ -> False) True
--
-- A more general example is `any`, which applies a predicate to each input
-- element in turn until it finds the first one for which the predicate is
-- true, at which point it returns success.  If, in an infinite input stream
-- the predicate is false for all the elements, `any` will not terminate,
-- but since it runs in constant space, it typically won't run out of memory,
-- it'll just loop forever.

--------------

-- $shortlist
--
-- The full list of short-circuit folds in this module is:
--
-- * Boolean predicate folds.
--   These functions examine elements strictly until a condition is met,
--   but then return a result ignoring the rest (lazy in the tail).  These
--   may loop forever given an unbounded input where no elements satisy the
--   termination condition.
--
--     @
--     `null`    :: Foldable t => t a -> Bool
--     `elem`    :: Foldable t => Eq a => a -> t a -> Bool
--     `notElem` :: (Foldable t, Eq a) => a -> t a -> Bool
--     `and`     :: Foldable t => t Bool -> Bool
--     `or`      :: Foldable t => t Bool -> Bool
--     `find`    :: Foldable t => (a -> Bool) -> t a -> Maybe a
--     `any`     :: Foldable t => (a -> Bool) -> t a -> Bool
--     `all`     :: Foldable t => (a -> Bool) -> t a -> Bool
--     @
--
-- * Many instances of '<|>' (e.g. the 'Maybe' instance) are conditionally
--   lazy, and use or don't use their second argument depending on the value
--   of the first.  These are used with the folds below, which terminate as
--   early as possible, but otherwise generally keep going.  Some instances
--   (e.g. for List) are always strict, but the result is lazy in the tail
--   of the output, so that `asum` for a list of lists is in fact corecursive.
--   These folds are defined in terms of `foldr`.
--
--     @
--     `asum` :: (Foldable t, Alternative f) => t (f a) -> f a
--     `msum` :: (Foldable t, MonadPlus m) => t (m a) -> m a
--     @
--
-- * Likewise, the '*>' operator in some `Applicative` functors, and '>>'
--   in some monads are conditionally lazy and can /short-circuit/ a chain of
--   computations.  The below folds will terminate as early as possible, but
--   even infinite loops can be productive here, when evaluated solely for
--   their stream of IO side-effects.  See "Data.Traversable#validation"
--   for some additional discussion.
--
--     @
--     `traverse_`  :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
--     `for_`       :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
--     `sequenceA_` :: (Foldable t, Applicative f) => t (f a) -> f ()
--     `mapM_`      :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
--     `forM_`      :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
--     `sequence_`  :: (Foldable t, Monad m) => t (m a) -> m ()
--     @
--
-- * Finally, there's one more special case, which can short-circuit when the
--   monad @m@ is a 'MonadPlus', and the update function conditionally calls
--   'mzero'.  The monadic result is a strict left fold of the inputs when the
--   monad's bind operator is strict in its first argument.  And yet the
--   monadic actions of this ostensibly left fold are sequenced via the lazy
--   `foldr`, which allows it to short-circuit early.  The monadic side-effects
--   are evaluated in left-to-right order.
--
--     @
--     `foldlM` :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
--     @
--
--     For a structure @xs@ with elements @(a, b, c, &#x2026;)@, the default
--     definition of @(foldlM f xs z)@ expands (via `foldr`) to:
--
--     @
--     return z >>= (g a >>= (g b >>= (g c >>= ...)))
--       where g element acc = f acc element -- i.e. g = flip f
--     @
--
--     The fold is not strict in the accumulator, unless @f@ is.  But even when
--     @f@ is strict, if the Monad's bind operator is not strict in its right
--     argument the chain of monadic actions can short-circuit:
--
--     >>> :set -XBangPatterns
--     >>> import Control.Monad
--     >>> import Control.Monad.Trans
--     >>> import Control.Monad.Trans.Maybe
--     >>> import Data.Foldable
--     >>> let f !_ e = when (e > 3) mzero >> lift (print e)
--     >>> runMaybeT $ foldlM f () [0..]
--     0
--     1
--     2
--     3
--     Nothing
--
--     Contrast this with `foldrM`, which uses `foldl` to sequence the effects,
--     and therefore diverges (running out of space) when given an unbounded
--     input structure.  The short-circuit condition is never reached
--
--     >>> let f e _ = when (e > 3) mzero >> lift (print e)
--     >>> runMaybeT $ foldrM f () [0..]
--     ...hangs...
--
--     If the update function is changed to short-circuit on the initial
--     elements and the structure is finite, `foldrM` will run effects and
--     produce results in reverse order:
--
--     >>> let f e _ = when (e < 3) mzero >> lift (print e)
--     >>> runMaybeT $ foldrM f () [0..5]
--     5
--     4
--     3
--     Nothing

--------------

-- $hybrid
--
-- The below folds, are neither strict reductions that produce a final answer
-- in constant space, nor lazy corecursions, and so have limited applicability.
-- They do have specialised uses, but are best avoided when in doubt.
--
-- @
--   `foldr'` :: (a -> b -> b) -> b -> t a -> b
--   `foldl` :: (b -> a -> b) -> b -> t a -> b
--   `foldl1` :: (a -> a -> a) -> t a -> a
--   `foldrM` :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
-- @

--------------

-- $instances
--
-- For many structures reasonably efficient @Foldable@ instances can be derived
-- automatically, by enabling the @DeriveFoldable@ GHC extension.  When this
-- works, it is generally not necessary to define a custom instance by hand.
-- Though in some cases one may be able to get slightly faster hand-tuned code,
-- care is required to avoid producing slower code, or code that is not
-- sufficiently lazy, strict or /lawful/.
--
-- The hand-crafted intances can get away with only defining one of 'foldr' or
-- 'foldMap'.  All the other methods have default definitions in terms of one
-- of these.  The default definitions have the expected strictness and the
-- expected asymptotic runtime and space costs, modulo small constant factors.
-- If you choose to hand-tune, benchmarking is advised to see whether you're
-- doing better than the default derived implementations, plus careful tests to
-- ensure that the custom methods are correct.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be:
--
-- > instance Foldable Tree where
-- >    foldr f z Empty = z
-- >    foldr f z (Leaf x) = f x z
-- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
--
-- The 'Node' case is a right fold of the left subtree whose initial
-- value is a right fold of the rest of the tree.
--
-- For example, when `f` is '(:)', all three cases return an immediate
-- value, respectively @z@ or a /cons cell/ holding @x@ or @l@, with the
-- remainder the structure, if any, encapsulated in a lazy thunk.
-- This meets the expected efficient [corecursive](#corec) behaviour
-- of 'foldr'.
--
-- Alternatively, one could define @foldMap@:
--
-- > instance Foldable Tree where
-- >    foldMap f Empty = mempty
-- >    foldMap f (Leaf x) = f x
-- >    foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r
--
-- And indeed some efficiency may be gained by directly defining both,
-- avoiding some indirection in the default definitions that express
-- one in terms of the other.  If you implement just one, likely 'foldr'
-- is the better choice.
--
-- The fact that `foldl'` can be reasonably efficiently defined in terms
-- of 'foldr' is one of the more surprising features of @Foldable@.  It
-- may be instructive to take a look at how this works.

--------------

-- $strictlazy
--
-- #strictlazy#
--
-- The left fold:
--
-- @
-- foldl' f z [a, b, &#x2026;, x, y]
-- @
--
-- can be expanded as:
--
-- @
-- id . g y . g x . &#x2026; . g b . g a $ z
-- \ \ where g = flip f
-- @
--
-- In which to maintain the expected strictness we need to perform function
-- application eagerly, and composition lazily.  To that end we introduce a new
-- function @f'@ which maps each element @x@ to an eager application of @g x@
-- to its argument, followed by an application of a lazily computed composition
-- (@k@) of the @g e@ functions for the remaining elements @e@:
--
-- > f' x k z = k $! (g x) z = k $! f z x
--
-- We see that a lazy 'foldr' of the @g e@ endomorphisms, with @f'@ as as the
-- update function, in fact yields a strict left fold, that avoids building a
-- deep chain of intermediate thunks:
--
-- > foldl' f z0 xs = foldr f' id xs z0
-- >   where f' x k z = k $! f z x
--
-- The function applied to @z0@ is built corecursively, and its terms are
-- applied eagerly to the accumulator before further terms are applied to
-- the result.  So, as promised, this will run in constant space, and GHC
-- is able to optimise this to an efficient loop.

--------------

-- $multipass
--
-- In applications where you want to compute a composite function of a
-- structure, which requires more than one aggregate as an input, it is
-- generally best to compute all the aggregates in a single pass, rather
-- than to traverse the same structure repeatedly.
--
-- The [@foldl@](http://hackage.haskell.org/package/foldl) package implements a
-- robust general framework for dealing with this situation.  If you choose to
-- to do it yourself, with a bit of care, the simplest cases are not difficult
-- to handle directly.  You just need to accumulate the individual aggregates
-- as __strict__ components of a single data type, and then apply a final
-- transformation to it to extract the composite result.  For example,
-- computing an average requires computing both the 'sum' and the 'length' of a
-- (non-empty) structure and dividing the sum by the length:
--
-- > import Data.Foldable (foldl')
-- >
-- > data PairS a b = P !a !b -- strict pair
-- >
-- > -- | Compute sum and length in a single pass, then reduce to the average.
-- > average :: (Foldable f, Fractional a) => f a -> a
-- > average = pairToFrac . foldl' f z
-- >   where
-- >     z = P 0 (0 :: Int)
-- >     f (P s l) a = P (s+a) (l+1)
-- >     pairToFrac (P s l) = s / fromIntegral l
--
-- The above example is somewhat contrived, some structures keep track of
-- their length internally, and can return it in @O(1)@ time, so this
-- particular recipe for averages is not always the most efficient.
-- In general, composite aggregate functions of large structures benefit
-- from single-pass reduction.

--------------

-- $laws
--
-- @Foldable@ instances are expected to satisfy the following laws:
--
-- > foldr f z t = appEndo (foldMap (Endo . f) t ) z
--
-- > foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
--
-- > fold = foldMap id
--
-- > length = getSum . foldMap (Sum . const 1)
--
-- @sum@, @product@, @maximum@, and @minimum@ should all be essentially
-- equivalent to @foldMap@ forms, such as
--
-- > sum     = getSum     . foldMap' Sum
-- > product = getProduct . foldMap' Product
--
-- but are generally more efficient when defined more directly as:
--
-- > sum = foldl' (+) 0
-- > sum = foldl' (*) 1
--
-- If the type is also a 'Functor' instance, it should satisfy
--
-- > foldMap f = fold . fmap f
--
-- which implies that
--
-- > foldMap f . fmap g = foldMap (f . g)
--

--------------

-- $notes
--
-- #notes#
-- The absence of a 'Functor' superclass allows
-- @Foldable@ structures to impose constraints on their element types.  Thus,
-- Sets are @Foldable@, even though @Set@ imposes an 'Ord' constraint on its
-- elements (this precludes defining a @Functor@ instance for @Set@).
--
-- The @Foldable@ class makes it possible to use idioms familiar from the List
-- type with container structures that are better suited to the task at hand.
-- This allows a user to substitute more appropriate @Foldable@ data types
-- for Lists without requiring new idioms (see [[1\]](#uselistsnot) for when
-- not to use lists).
--
-- The more general methods of the @Foldable@ class are now exported by the
-- "Prelude" in place of the original List-specific methods (see the
-- [FTP Proposal](https://wiki.haskell.org/Foldable_Traversable_In_Prelude)).
-- The List-specific variants are for now still available in "GHC.OldList", but
-- that module is intended only as a transitional aid, and may be removed in
-- the future.
--
-- Surprises can arise from the @Foldable@ instance of the 2-tuple @(a,)@ which
-- now behaves as a 1-element @Foldable@ container in its second slot.  In
-- contexts where a specific monomorphic type is expected, and you want to be
-- able to rely on type errors to guide refactoring, it may make sense to
-- define and use less-polymorphic variants of some of the @Foldable@ methods.
--
-- Below are two examples showing a definition of a reusable less-polymorphic
-- 'sum' and a one-off in-line specialisation of 'length':
--
-- > {-# LANGUAGE TypeApplications #-}
-- >
-- > mySum :: Num a => [a] -> a
-- > mySum = sum
-- >
-- > type SlowVector a = [a]
-- > slowLength :: SlowVector -> Int
-- > slowLength v = length @[] v
--
-- In both cases, if the data type to which the function is applied changes
-- to something other than a list, the call-site will no longer compile until
-- appropriate changes are made.

--------------

-- $also
--
--  * [1] #uselistsnot# \"When You Should Use Lists in Haskell (Mostly, You Should Not)\",
--    by Johannes Waldmann,
--    in arxiv.org, Programming Languages (cs.PL), at
--    <https://arxiv.org/abs/1808.08329>.
--
--  * [2] \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/#iterator>.
