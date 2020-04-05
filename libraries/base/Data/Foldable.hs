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
    -- entire input list must be traversed. This means that 'foldl'' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use 'foldl'' instead of 'foldl'. The reason for this is that latter does
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
       getMax . foldMap (Max #. (Just :: a -> Maybe a))

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
       getMin . foldMap (Min #. (Just :: a -> Maybe a))

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
    sum = getSum #. foldMap Sum

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
maximumBy cmp = foldl1 max'
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
-- >>> minimumBy (compare `on` length) ["Hello", "World", "!", "Longest", "bar"]
-- "!"

-- See Note [maximumBy/minimumBy space usage]
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

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
minimumBy to foldl1 solves the issue, as GHC's strictness analysis can then
make these functions only use O(1) stack space. It is perhaps not the optimal
way to fix this problem, as there are other conceivable data structures
(besides lists) which might benefit from specialized implementations for
maximumBy and minimumBy (see
https://gitlab.haskell.org/ghc/ghc/issues/10830#note_129843 for a further
discussion). But using foldl1 is at least always better than using foldr1, so
GHC has chosen to adopt that approach for now.
-}
