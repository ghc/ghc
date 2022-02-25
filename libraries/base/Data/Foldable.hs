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
-- Stability   :  stable
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

    -- ** Expectation of efficient left-to-right iteration
    -- $chirality

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

    -- ** Generative Recursion
    -- $generative

    -- ** Avoiding multi-pass folds
    -- $multipass

    -- * Defining instances
    -- $instances

    -- *** Being strict by being lazy
    -- $strictlazy

    -- * Laws
    -- $laws

    -- * Notes
    -- $notes

    -- ** Generally linear-time `elem`
    -- $linear

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
import GHC.Tuple (Solo (..))
import GHC.Num  ( Num(..) )

-- $setup
-- >>> :set -XDeriveFoldable
-- >>> import Prelude
-- >>> import Data.Monoid (Product (..), Sum (..))
-- >>> data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Foldable)

infix  4 `elem`, `notElem`

-- XXX: Missing haddock feature.  Links to anchors in other modules
-- don't have a sensible way to name the link within the module itself.
-- Thus, the below "Data.Foldable#overview" works well when shown as
-- @Data.Foldable@ from other modules, but in the home module it should
-- be possible to specify alternative link text. :-(

-- | The Foldable class represents data structures that can be reduced to a
-- summary value one element at a time.  Strict left-associative folds are a
-- good fit for space-efficient reduction, while lazy right-associative folds
-- are a good fit for corecursive iteration, or for folds that short-circuit
-- after processing an initial subsequence of the structure's elements.
--
-- Instances can be derived automatically by enabling the @DeriveFoldable@
-- extension.  For example, a derived instance for a binary tree might be:
--
-- > {-# LANGUAGE DeriveFoldable #-}
-- > data Tree a = Empty
-- >             | Leaf a
-- >             | Node (Tree a) a (Tree a)
-- >     deriving Foldable
--
-- A more detailed description can be found in the __Overview__ section of
-- "Data.Foldable#overview".
--
-- For the class laws see the __Laws__ section of "Data.Foldable#laws".
--
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Given a structure with elements whose type is a 'Monoid', combine them
    -- via the monoid's @('<>')@ operator.  This fold is right-associative and
    -- lazy in the accumulator.  When you need a strict left-associative fold,
    -- use 'foldMap'' instead, with 'id' as the map.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- >>> fold [[1, 2, 3], [4, 5], [6], []]
    -- [1,2,3,4,5,6]
    --
    -- >>> fold $ Node (Leaf (Sum 1)) (Sum 3) (Leaf (Sum 5))
    -- Sum {getSum = 9}
    --
    -- Folds of unbounded structures do not terminate when the monoid's
    -- @('<>')@ operator is strict:
    --
    -- >>> fold (repeat Nothing)
    -- * Hangs forever *
    --
    -- Lazy corecursive folds of unbounded structures are fine:
    --
    -- >>> take 12 $ fold $ map (\i -> [i..i+2]) [0..]
    -- [0,1,2,1,2,3,2,3,4,3,4,5]
    -- >>> sum $ take 4000000 $ fold $ map (\i -> [i..i+2]) [0..]
    -- 2666668666666
    --
    fold :: Monoid m => t m -> m
    {-# INLINE fold #-}
    fold = foldMap id

    -- | Map each element of the structure into a monoid, and combine the
    -- results with @('<>')@.  This fold is right-associative and lazy in the
    -- accumulator.  For strict left-associative folds consider 'foldMap''
    -- instead.
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
    -- When a Monoid's @('<>')@ is lazy in its second argument, 'foldMap' can
    -- return a result even from an unbounded structure.  For example, lazy
    -- accumulation enables "Data.ByteString.Builder" to efficiently serialise
    -- large data structures and produce the output incrementally:
    --
    -- >>> import qualified Data.ByteString.Lazy as L
    -- >>> import qualified Data.ByteString.Builder as B
    -- >>> let bld :: Int -> B.Builder; bld i = B.intDec i <> B.word8 0x20
    -- >>> let lbs = B.toLazyByteString $ foldMap bld [0..]
    -- >>> L.take 64 lbs
    -- "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"
    --
    foldMap :: Monoid m => (a -> m) -> t a -> m
    {-# INLINE foldMap #-}
    -- This INLINE allows more list functions to fuse.  See #9848.
    foldMap f = foldr (mappend . f) mempty

    -- | A left-associative variant of 'foldMap' that is strict in the
    -- accumulator.  Use this method for strict reduction when partial
    -- results are merged via @('<>')@.
    --
    -- ==== __Examples__
    --
    -- Define a 'Monoid' over finite bit strings under 'xor'.  Use it to
    -- strictly compute the `xor` of a list of 'Int' values.
    --
    -- >>> :set -XGeneralizedNewtypeDeriving
    -- >>> import Data.Bits (Bits, FiniteBits, xor, zeroBits)
    -- >>> import Data.Foldable (foldMap')
    -- >>> import Numeric (showHex)
    -- >>>
    -- >>> newtype X a = X a deriving (Eq, Bounded, Enum, Bits, FiniteBits)
    -- >>> instance Bits a => Semigroup (X a) where X a <> X b = X (a `xor` b)
    -- >>> instance Bits a => Monoid    (X a) where mempty     = X zeroBits
    -- >>>
    -- >>> let bits :: [Int]; bits = [0xcafe, 0xfeed, 0xdeaf, 0xbeef, 0x5411]
    -- >>> (\ (X a) -> showString "0x" . showHex a $ "") $ foldMap' X bits
    -- "0x42"
    --
    -- @since 4.13.0.0
    foldMap' :: Monoid m => (a -> m) -> t a -> m
    foldMap' f = foldl' (\ acc a -> acc <> f a) mempty

    -- | Right-associative fold of a structure, lazy in the accumulator.
    --
    -- In the case of lists, 'foldr', when applied to a binary operator, a
    -- starting value (typically the right-identity of the operator), and a
    -- list, reduces the list using the binary operator, from right to left:
    --
    -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
    --
    -- Note that since the head of the resulting expression is produced by an
    -- application of the operator to the first element of the list, given an
    -- operator lazy in its right argument, 'foldr' can produce a terminating
    -- expression from an unbounded list.
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
    -- >>> foldr (\c acc -> acc ++ [c]) "foo" ['a', 'b', 'c', 'd']
    -- "foodcba"
    --
    -- ===== Infinite structures
    --
    -- ⚠️ Applying 'foldr' to infinite structures usually doesn't terminate.
    --
    -- It may still terminate under one of the following conditions:
    --
    -- * the folding function is short-circuiting
    -- * the folding function is lazy on its second argument
    --
    -- ====== Short-circuiting
    --
    -- @('||')@ short-circuits on 'True' values, so the following terminates
    -- because there is a 'True' value finitely far from the left side:
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
    -- Applying 'foldr' to infinite structures terminates when the operator is
    -- lazy in its second argument (the initial accumulator is never used in
    -- this case, and so could be left 'undefined', but @[]@ is more clear):
    --
    -- >>> take 5 $ foldr (\i acc -> i : fmap (+3) acc) [] (repeat 1)
    -- [1,4,7,10,13]
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    -- | 'foldr'' is a variant of 'foldr' that performs strict reduction from
    -- right to left, i.e. starting with the right-most element.  The input
    -- structure /must/ be finite, otherwise 'foldr'' runs out of space
    -- (/diverges/).
    --
    -- If you want a strict right fold in constant space, you need a structure
    -- that supports faster than /O(n)/ access to the right-most element, such
    -- as @Seq@ from the @containers@ package.
    --
    -- This method does not run in constant space for structures such as lists
    -- that don't support efficient right-to-left iteration and so require
    -- /O(n)/ space to perform right-to-left reduction.  Use of this method
    -- with such a structure is a hint that the chosen structure may be a poor
    -- fit for the task at hand.  If the order in which the elements are
    -- combined is not important, use 'foldl'' instead.
    --
    -- @since 4.6.0.0
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z0 = \ xs ->
        foldl (\ (k::b->b) (x::a) -> oneShot (\ (z::b) -> z `seq` k (f x z)))
              (id::b->b) xs z0
    -- Mirror image of 'foldl''.

    -- | Left-associative fold of a structure, lazy in the accumulator.  This
    -- is rarely what you want, but can work well for structures with efficient
    -- right-to-left sequencing and an operator that is lazy in its left
    -- argument.
    --
    -- In the case of lists, 'foldl', when applied to a binary operator, a
    -- starting value (typically the left-identity of the operator), and a
    -- list, reduces the list using the binary operator, from left to right:
    --
    -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
    --
    -- Note that to produce the outermost application of the operator the
    -- entire input list must be traversed.  Like all left-associative folds,
    -- 'foldl' will diverge if given an infinite list.
    --
    -- If you want an efficient strict left-fold, you probably want to use
    -- 'foldl'' instead of 'foldl'.  The reason for this is that the latter
    -- does not force the /inner/ results (e.g. @z \`f\` x1@ in the above
    -- example) before applying them to the operator (e.g. to @(\`f\` x2)@).
    -- This results in a thunk chain /O(n)/ elements long, which then must be
    -- evaluated from the outside-in.
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to:
    --
    -- @foldl f z = 'List.foldl' f z . 'toList'@
    --
    -- ==== __Examples__
    --
    -- The first example is a strict fold, which in practice is best performed
    -- with 'foldl''.
    --
    -- >>> foldl (+) 42 [1,2,3,4]
    -- 52
    --
    -- Though the result below is lazy, the input is reversed before prepending
    -- it to the initial accumulator, so corecursion begins only after traversing
    -- the entire input string.
    --
    -- >>> foldl (\acc c -> c : acc) "abcd" "efgh"
    -- "hgfeabcd"
    --
    -- A left fold of a structure that is infinite on the right cannot
    -- terminate, even when for any finite input the fold just returns the
    -- initial accumulator:
    --
    -- >>> foldl (\a _ -> a) 0 $ repeat 1
    -- * Hangs forever *
    --
    -- WARNING: When it comes to lists, you always want to use either 'foldl'' or 'foldr' instead.
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    -- There's no point mucking around with coercions here,
    -- because flip forces us to build a new function anyway.

    -- | Left-associative fold of a structure but with strict application of
    -- the operator.
    --
    -- This ensures that each step of the fold is forced to Weak Head Normal
    -- Form before being applied, avoiding the collection of thunks that would
    -- otherwise occur.  This is often what you want to strictly reduce a
    -- finite structure to a single strict result (e.g. 'sum').
    --
    -- For a general 'Foldable' structure this should be semantically identical
    -- to,
    --
    -- @foldl' f z = 'List.foldl'' f z . 'toList'@
    --
    -- @since 4.6.0.0
    foldl' :: (b -> a -> b) -> b -> t a -> b
    {-# INLINE foldl' #-}
    foldl' f z0 = \ xs ->
        foldr (\ (x::a) (k::b->b) -> oneShot (\ (z::b) -> z `seq` k (f z x)))
              (id::b->b) xs z0
    --
    -- We now force the accumulator `z` rather than the value computed by the
    -- operator `k`, this matches the documented strictness.
    --
    -- For the rationale for the arity reduction from 3 to 2, inlining, etc.
    -- see Note [Definition of foldl'] in GHC.List.

    -- | A variant of 'foldr' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- This function is non-total and will raise a runtime exception if the
    -- structure happens to be empty.
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
    -- This function is non-total and will raise a runtime exception if the
    -- structure happens to be empty.
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

    -- | List of elements of a structure, from left to right.  If the entire
    -- list is intended to be reduced via a fold, just fold the structure
    -- directly bypassing the list.
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

    -- | Test whether the structure is empty.  The default implementation is
    -- Left-associative and lazy in both the initial element and the
    -- accumulator.  Thus optimised for structures where the first element can
    -- be accessed in constant time.  Structures where this is not the case
    -- should have a non-default implementation.
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
    -- 'null' is expected to terminate even for infinite structures.
    -- The default implementation terminates provided the structure
    -- is bounded on the left (there is a leftmost element).
    --
    -- >>> null [1..]
    -- False
    --
    -- @since 4.8.0.0
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    -- | Returns the size/length of a finite structure as an 'Int'.  The
    -- default implementation just counts elements starting with the leftmost.
    -- Instances for structures that can compute the element count faster
    -- than via element-by-element counting, should provide a specialised
    -- implementation.
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
    -- For infinite structures, the default implementation of 'elem'
    -- terminates if the sought-after value exists at a finite distance
    -- from the left side of the structure:
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
    -- This function is non-total and will raise a runtime exception if the
    -- structure happens to be empty.  A structure that supports random access
    -- and maintains its elements in order should provide a specialised
    -- implementation to return the maximum in faster than linear time.
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
    -- WARNING: This function is partial for possibly-empty structures like lists.
    --
    -- @since 4.8.0.0
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
       getMax . foldMap' (Max #. (Just :: a -> Maybe a))
    {-# INLINEABLE maximum #-}

    -- | The least element of a non-empty structure.
    --
    -- This function is non-total and will raise a runtime exception if the
    -- structure happens to be empty.  A structure that supports random access
    -- and maintains its elements in order should provide a specialised
    -- implementation to return the minimum in faster than linear time.
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
    -- WARNING: This function is partial for possibly-empty structures like lists.
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
    foldr'  = List.foldr'
    foldr1  = List.foldr1
    foldMap = (mconcat .) . map -- See Note [Monoidal list folds]
    fold    = mconcat           -- See Note [Monoidal list folds]
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

-- | @since 4.15
deriving instance Foldable Solo

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

-- | Right-to-left monadic fold over the elements of a structure.
--
-- Given a structure @t@ with elements @(a, b, c, ..., x, y)@, the result of
-- a fold with an operator function @f@ is equivalent to:
--
-- > foldrM f z t = do
-- >     yy <- f y z
-- >     xx <- f x yy
-- >     ...
-- >     bb <- f b cc
-- >     aa <- f a bb
-- >     return aa -- Just @return z@ when the structure is empty
--
-- For a Monad @m@, given two functions @f1 :: a -> m b@ and @f2 :: b -> m c@,
-- their Kleisli composition @(f1 >=> f2) :: a -> m c@ is defined by:
--
-- > (f1 >=> f2) a = f1 a >>= f2
--
-- Another way of thinking about @foldrM@ is that it amounts to an application
-- to @z@ of a Kleisli composition:
--
-- > foldrM f z t = f y >=> f x >=> ... >=> f b >=> f a $ z
--
-- The monadic effects of @foldrM@ are sequenced from right to left, and e.g.
-- folds of infinite lists will diverge.
--
-- If at some step the bind operator @('>>=')@ short-circuits (as with, e.g.,
-- 'mzero' in a 'MonadPlus'), the evaluated effects will be from a tail of the
-- element sequence.  If you want to evaluate the monadic effects in
-- left-to-right order, or perhaps be able to short-circuit after an initial
-- sequence of elements, you'll need to use `foldlM` instead.
--
-- If the monadic effects don't short-circuit, the outermost application of
-- @f@ is to the leftmost element @a@, so that, ignoring effects, the result
-- looks like a right fold:
--
-- > a `f` (b `f` (c `f` (... (x `f` (y `f` z))))).
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let f i acc = do { print i ; return $ i : acc }
-- >>> foldrM f [] [0..3]
-- 3
-- 2
-- 1
-- 0
-- [0,1,2,3]
--
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c k x z = f x z >>= k
        {-# INLINE c #-}

-- | Left-to-right monadic fold over the elements of a structure.
--
-- Given a structure @t@ with elements @(a, b, ..., w, x, y)@, the result of
-- a fold with an operator function @f@ is equivalent to:
--
-- > foldlM f z t = do
-- >     aa <- f z a
-- >     bb <- f aa b
-- >     ...
-- >     xx <- f ww x
-- >     yy <- f xx y
-- >     return yy -- Just @return z@ when the structure is empty
--
-- For a Monad @m@, given two functions @f1 :: a -> m b@ and @f2 :: b -> m c@,
-- their Kleisli composition @(f1 >=> f2) :: a -> m c@ is defined by:
--
-- > (f1 >=> f2) a = f1 a >>= f2
--
-- Another way of thinking about @foldlM@ is that it amounts to an application
-- to @z@ of a Kleisli composition:
--
-- > foldlM f z t =
-- >     flip f a >=> flip f b >=> ... >=> flip f x >=> flip f y $ z
--
-- The monadic effects of @foldlM@ are sequenced from left to right.
--
-- If at some step the bind operator @('>>=')@ short-circuits (as with, e.g.,
-- 'mzero' in a 'MonadPlus'), the evaluated effects will be from an initial
-- segment of the element sequence.  If you want to evaluate the monadic
-- effects in right-to-left order, or perhaps be able to short-circuit after
-- processing a tail of the sequence of elements, you'll need to use `foldrM`
-- instead.
--
-- If the monadic effects don't short-circuit, the outermost application of
-- @f@ is to the rightmost element @y@, so that, ignoring effects, the result
-- looks like a left fold:
--
-- > ((((z `f` a) `f` b) ... `f` w) `f` x) `f` y
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let f a e = do { print e ; return $ e : a }
-- >>> foldlM f [] [0..3]
-- 0
-- 1
-- 2
-- 3
-- [3,2,1,0]
--
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c x k z = f z x >>= k
        {-# INLINE c #-}

-- | Map each element of a structure to an 'Applicative' action, evaluate these
-- actions from left to right, and ignore the results.  For a version that
-- doesn't ignore the results see 'Data.Traversable.traverse'.
--
-- 'traverse_' is just like 'mapM_', but generalised to 'Applicative' actions.
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

-- | 'for_' is 'traverse_' with its arguments flipped.  For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.  This
-- is 'forM_' generalised to 'Applicative' actions.
--
-- 'for_' is just like 'forM_', but generalised to 'Applicative' actions.
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
-- these actions from left to right, and ignore the results.  For a
-- version that doesn't ignore the results see
-- 'Data.Traversable.mapM'.
--
-- 'mapM_' is just like 'traverse_', but specialised to monadic actions.
--
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x >> k
        {-# INLINE c #-}

-- | 'forM_' is 'mapM_' with its arguments flipped.  For a version that
-- doesn't ignore the results see 'Data.Traversable.forM'.
--
-- 'forM_' is just like 'for_', but specialised to monadic actions.
--
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = flip mapM_

-- | Evaluate each action in the structure from left to right, and
-- ignore the results.  For a version that doesn't ignore the results
-- see 'Data.Traversable.sequenceA'.
--
-- 'sequenceA_' is just like 'sequence_', but generalised to 'Applicative'
-- actions.
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
-- and ignore the results.  For a version that doesn't ignore the
-- results see 'Data.Traversable.sequence'.
--
-- 'sequence_' is just like 'sequenceA_', but specialised to monadic
-- actions.
--
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c m k = m >> k
        {-# INLINE c #-}

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- 'asum' is just like 'msum', but generalised to 'Alternative'.
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
--
-- 'msum' is just like 'asum', but specialised to 'MonadPlus'.
--
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
-- >>> concat (Left 42)
-- []
--
-- >>> concat [[1, 2, 3], [4, 5], [6], []]
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
-- >>> concatMap (take 3) (Just [1..])
-- [1,2,3]
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
-- >>> and (False : repeat True) -- Infinite list [False,True,True,True,...
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
-- >>> or (True : repeat False) -- Infinite list [True,False,False,False,...
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
--
-- WARNING: This function is partial for possibly-empty structures like lists.

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
--
-- WARNING: This function is partial for possibly-empty structures like lists.

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
foldr1.  This was problematic for space usage, as the semantics of maximumBy
and minimumBy essentially require that they examine every element of the
data structure.  Using foldr1 to examine every element results in space usage
proportional to the size of the data structure.  For the common case of lists,
this could be particularly bad (see #10830).

For the common case of lists, switching the implementations of maximumBy and
minimumBy to foldl1 solves the issue, assuming GHC's strictness analysis can then
make these functions only use O(1) stack space.  As of base 4.16, we have
switched to employing foldl' over foldl1, not relying on GHC's optimiser.  See
https://gitlab.haskell.org/ghc/ghc/-/issues/17867 for more context.
-}

{-
Note [Monoidal list folds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Folds of lists of monoid elements should generally use 'mconcat', rather than
@foldr mappend mempty@.  This allows specialized mconcat implementations an
opportunity to combine elements efficiently.  For example, `mappend` of strict
`Text` and `ByteString` values typically needs to reallocate and copy the
existing data, making incremental construction expensive (likely quadratic in
the number of elements combined).  The `mconcat` implementations for `Text` and
`ByteString` preallocate the required storage, and then combine all the list
elements in a single pass.
-}

--------------

-- $overview
--
-- #overview#
-- The Foldable class generalises some common "Data.List" functions to
-- structures that can be reduced to a summary value one element at a time.
--
-- == Left and right folds
--
-- #leftright#
-- The contribution of each element to the final result is combined with an
-- accumulator via a suitable /operator/.  The operator may be explicitly
-- provided by the caller as with `foldr` or may be implicit as in `length`.
-- In the case of `foldMap`, the caller provides a function mapping each
-- element into a suitable 'Monoid', which makes it possible to merge the
-- per-element contributions via that monoid's `mappend` function.
--
-- A key distinction is between left-associative and right-associative
-- folds:
--
-- * In left-associative folds the accumulator is a partial fold over the
--   elements that __precede__ the current element, and is passed to the
--   operator as its first (left) argument.  The outermost application of the
--   operator merges the contribution of the last element of the structure with
--   the contributions of all its predecessors.
--
-- * In right-associative folds the accumulator is a partial fold over the
--   elements that __follow__ the current element, and is passed to the
--   operator as its second (right) argument.  The outermost application of
--   the operator merges the contribution of the first element of the structure
--   with the contributions of all its successors.
--
-- These two types of folds are typified by the left-associative strict
-- 'foldl'' and the right-associative lazy `foldr`.
--
-- @
-- 'foldl'' :: Foldable t => (b -> a -> b) -> b -> t a -> b
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
-- The first argument of both is an explicit /operator/ that merges the
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
-- * __'foldl''__ takes an operator argument of the form:
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
--     Since 'foldl'' is strict in the accumulator, this is always
--     a [strict](#strict) reduction with no opportunity for early return or
--     intermediate results.  The structure must be finite, since no result is
--     returned until the last element is processed.  The advantage of
--     strictness is space efficiency: the final result can be computed without
--     storing a potentially deep stack of lazy intermediate results.
--
-- * __`foldr`__ takes an operator argument of the form:
--
--     @
--     f :: a -- current element
--       -> b -- accumulated fold of the remaining elements
--       -> b -- updated fold, inclusive of current element
--     @
--
--     the result of the fold is:
--
--     @f a . f b . f c . &#x2026; $ z@
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
--     When the operator is always strict in its second argument, 'foldl'' is
--     generally a better choice than `foldr`.  When `foldr` is called with a
--     strict operator, evaluation cannot begin until the last element is
--     reached, by which point a deep stack of pending function applications
--     may have been built up in memory.
--

-- $chirality
--
-- #chirality#
-- Foldable structures are generally expected to be efficiently iterable from
-- left to right. Right-to-left iteration may be substantially more costly, or
-- even impossible (as with, for example, infinite lists).  The text in the
-- sections that follow that suggests performance differences between
-- left-associative and right-associative folds assumes /left-handed/
-- structures in which left-to-right iteration is cheaper than right-to-left
-- iteration.
--
-- In finite structures for which right-to-left sequencing no less efficient
-- than left-to-right sequencing, there is no inherent performance distinction
-- between left-associative and right-associative folds.  If the structure's
-- @Foldable@ instance takes advantage of this symmetry to also make strict
-- right folds space-efficient and lazy left folds corecursive, one need only
-- take care to choose either a strict or lazy method for the task at hand.
--
-- Foldable instances for symmetric structures should strive to provide equally
-- performant left-associative and right-associative interfaces. The main
-- limitations are:
--
-- * The lazy 'fold', 'foldMap' and 'toList' methods have no right-associative
--   counterparts.
-- * The strict 'foldMap'' method has no left-associative counterpart.
--
-- Thus, for some foldable structures 'foldr'' is just as efficient as 'foldl''
-- for strict reduction, and 'foldl' may be just as appropriate for corecursive
-- folds as 'foldr'.
--
-- Finally, in some less common structures (e.g. /snoc/ lists) right to left
-- iterations are cheaper than left to right.  Such structures are poor
-- candidates for a @Foldable@ instance, and are perhaps best handled via their
-- type-specific interfaces.  If nevertheless a @Foldable@ instance is
-- provided, the material in the sections that follow applies to these also, by
-- replacing each method with one with the opposite associativity (when
-- available) and switching the order of arguments in the fold's /operator/.
--
-- You may need to pay careful attention to strictness of the fold's /operator/
-- when its strictness is different between its first and second argument.
-- For example, while @('+')@ is expected to be commutative and strict in both
-- arguments, the list concatenation operator @('++')@ is not commutative and
-- is only strict in the initial constructor of its first argument.  The fold:
--
-- > myconcat xs = foldr (\a b -> a ++ b) [] xs
--
-- is substantially cheaper (linear in the length of the consumed portion of
-- the final list, thus e.g. constant time/space for just the first element)
-- than:
--
-- > revconcat xs = foldr (\a b -> b ++ a) [] xs
--
-- In which the total cost scales up with both the number of lists combined and
-- the number of elements ultimately consumed.  A more efficient way to combine
-- lists in reverse order, is to use:
--
-- > revconcat = foldr (++) [] . reverse

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
--   additional input elements.  Lazy processing of the remaining elements
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
-- both the method chosen to perform the fold and on the operator passed to
-- that method (which may be implicit, as with the `mappend` method of a monoid
-- instance).
--
-- There are also hybrid cases, where the method and/or operator are not well
-- suited to the task at hand, resulting in a fold that fails to yield
-- incremental results until the entire input is processed, or fails to
-- strictly evaluate results as it goes, deferring all the work to the
-- evaluation of a large final thunk.  Such cases should be avoided, either by
-- selecting a more appropriate @Foldable@ method, or by tailoring the operator
-- to the chosen method.
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
-- 'foldl'') to perform an efficient reduction in constant space.
--
-- Conversely, an implementation of @Foldable@ for a new structure should
-- ensure that 'foldl'' actually performs a strict left-associative reduction.
--
-- The 'foldMap'' method is a special case of 'foldl'', in which the initial
-- accumulator is `mempty` and the operator is @mappend . f@, where @f@ maps
-- each input element into the 'Monoid' in question.  Therefore, 'foldMap'' is
-- an appropriate choice under essentially the same conditions as 'foldl'', and
-- its implementation for a given @Foldable@ structure should also be a strict
-- left-associative reduction.
--
-- While the examples below are not necessarily the most optimal definitions of
-- the intended functions, they are all cases in which 'foldMap'' is far more
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
-- * Provided the operator is strict in its left argument:
--
--     @'foldl'' :: Foldable t => (b -> a -> b) -> b -> t a -> b@
--
-- * Provided `mappend` is strict in its left argument:
--
--     @'foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m@
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
-- over the transformed input elements.  In such cases, it is important to
-- choose a @Foldable@ method that is lazy in the tail of the structure, such
-- as `foldr` (or `foldMap`, if the result @Monoid@ has a lazy `mappend` as
-- with e.g. ByteString Builders).
--
-- Conversely, an implementation of `foldr` for a structure that can
-- accommodate a large (and possibly unbounded) number of elements is expected
-- to be lazy in the tail of the input, allowing operators that are lazy in the
-- accumulator to yield intermediate results incrementally.  Such folds are
-- right-associative, with the tail of the stream returned as a lazily
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
--
-- A more complex example is concatenation of a list of lists expressed as a
-- nested right fold (bypassing @('++')@).  We can check that the definition is
-- indeed lazy by folding an infinite list of lists, and taking an initial
-- segment.
--
-- >>> myconcat = foldr (\x z -> foldr (:) z x) []
-- >>> take 15 $ myconcat $ map (\i -> [0..i]) [0..]
-- [0,0,1,0,1,2,0,1,2,3,0,1,2,3,4]
--
-- Of course in this case another way to achieve the same result is via a
-- list comprehension:
--
-- > myconcat xss = [x | xs <- xss, x <- xs]

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
-- Examples of short-circuit reduction include various boolean predicates that
-- test whether some or all the elements of a structure satisfy a given
-- condition.  Because these don't necessarily consume the entire list, they
-- typically employ `foldr` with an operator that is conditionally strict in
-- its second argument.  Once the termination condition is met the second
-- argument (tail of the input structure) is ignored.  No result is returned
-- until that happens.
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
--   may loop forever given an unbounded input where no elements satisfy the
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
-- * Many instances of @('<|>')@ (e.g. the 'Maybe' instance) are conditionally
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
-- * Likewise, the @('*>')@ operator in some `Applicative` functors, and @('>>')@
--   in some monads are conditionally lazy and can /short-circuit/ a chain of
--   computations.  The below folds will terminate as early as possible, but
--   even infinite loops can be productive here, when evaluated solely for
--   their stream of IO side-effects.  See "Data.Traversable#effectful"
--   for discussion of related functions.
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
-- * Finally, there's one more special case, `foldlM`:
--
--     @`foldlM` :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b@
--
--     The sequencing of monadic effects proceeds from left to right.  If at
--     some step the bind operator @('>>=')@ short-circuits (as with, e.g.,
--     'mzero' with a 'MonadPlus', or an exception with a 'MonadThrow', etc.),
--     then the evaluated effects will be from an initial portion of the
--     element sequence.
--
--     >>> :set -XBangPatterns
--     >>> import Control.Monad
--     >>> import Control.Monad.Trans.Class
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
--     Contrast this with `foldrM`, which sequences monadic effects from right
--     to left, and therefore diverges when folding an unbounded input
--     structure without ever having the opportunity to short-circuit.
--
--     >>> let f e _ = when (e > 3) mzero >> lift (print e)
--     >>> runMaybeT $ foldrM f () [0..]
--     ...hangs...
--
--     When the structure is finite `foldrM` performs the monadic effects from
--     right to left, possibly short-circuiting after processing a tail portion
--     of the element sequence.
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
-- 'foldr'' :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- 'foldl'  :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- 'foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
-- 'foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
-- @
--
-- The lazy left-folds (used corecursively) and 'foldrM' (used to sequence
-- actions right-to-left) can be performant in structures whose @Foldable@
-- instances take advantage of efficient right-to-left iteration to compute
-- lazy left folds outside-in from the rightmost element.
--
-- The strict 'foldr'' is the least likely to be useful, structures that
-- support efficient sequencing /only/ right-to-left are not common.

--------------

-- $instances
--
-- #instances#
-- For many structures reasonably efficient @Foldable@ instances can be derived
-- automatically, by enabling the @DeriveFoldable@ GHC extension.  When this
-- works, it is generally not necessary to define a custom instance by hand.
-- Though in some cases one may be able to get slightly faster hand-tuned code,
-- care is required to avoid producing slower code, or code that is not
-- sufficiently lazy, strict or /lawful/.
--
-- The hand-crafted instances can get away with only defining one of 'foldr' or
-- 'foldMap'.  All the other methods have default definitions in terms of one
-- of these.  The default definitions have the expected strictness and the
-- expected asymptotic runtime and space costs, modulo small constant factors.
-- If you choose to hand-tune, benchmarking is advised to see whether you're
-- doing better than the default derived implementations, plus careful tests to
-- ensure that the custom methods are correct.
--
-- Below we construct a @Foldable@ instance for a data type representing a
-- (finite) binary tree with depth-first traversal.
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
-- For example, when @f@ is @(':')@, all three cases return an immediate value,
-- respectively @z@ or a /cons cell/ holding @x@ or @l@, with the remainder the
-- structure, if any, encapsulated in a lazy thunk.  This meets the expected
-- efficient [corecursive](#corec) behaviour of 'foldr'.
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
-- A binary tree typically (when balanced, or randomly biased) provides equally
-- efficient access to its left and right subtrees.  This makes it possible to
-- define a `foldl` optimised for [corecursive](#corec) folds with operators
-- that are lazy in their first (left) argument.
--
-- > instance Foldable Tree where
-- >    foldr f z Empty = z
-- >    foldr f z (Leaf x) = f x z
-- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
-- >    --
-- >    foldMap f Empty = mempty
-- >    foldMap f (Leaf x) = f x
-- >    foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r
-- >    --
-- >    foldl f z Empty = z
-- >    foldl f z (Leaf x) = f z x
-- >    foldl f z (Node l k r) = foldl f (f (foldl f z l) k) r
--
-- Now left-to-right and right-to-left iteration over the structure
-- elements are equally efficient (note the mirror-order output when
-- using `foldl`):
--
-- >>> foldr (\e acc -> e : acc) [] (Node (Leaf 1) 2 (Leaf 3))
-- [1,2,3]
-- >>> foldl (\acc e -> e : acc) [] (Node (Leaf 1) 2 (Leaf 3))
-- [3,2,1]
--
-- We can carry this further, and define more non-default methods...
--
-- The structure definition actually admits trees that are unbounded on either
-- or both sides.  The only fold that can plausibly terminate for a tree
-- unbounded on both left and right is `null`, when defined as shown below.
-- The default definition in terms of `foldr` diverges if the tree is unbounded
-- on the left.  Here we define a variant that avoids travelling down the tree
-- to find the leftmost element and just examines the root node.
--
-- >    null Empty = True
-- >    null _     = False
--
-- This is a sound choice also for finite trees.
--
-- In practice, unbounded trees are quite uncommon, and can barely be said to
-- be @Foldable@.  They would typically employ breadth first traversal, and
-- would support only corecursive and short-circuit folds (diverge under strict
-- reduction).
--
-- Returning to simpler instances, defined just in terms of `foldr`, it is
-- somewhat surprising that a fairly efficient /default/ implementation of the
-- strict 'foldl'' is defined in terms of lazy `foldr` when only the latter is
-- explicitly provided by the instance.  It may be instructive to take a look
-- at how this works.

--------------

-- $strictlazy
--
-- #strictlazy#
--
-- Sometimes, it is useful for the result of applying 'foldr' to be a
-- /function/.  This is done by mapping the structure elements to functions
-- with the same argument and result types.  The per-element functions are then
-- composed to give the final result.
--
-- For example, we can /flip/ the strict left fold 'foldl'' by writing:
--
-- > foldl' f z xs = flippedFoldl' f xs z
--
-- with the function 'flippedFoldl'' defined as below, with 'seq' used to
-- ensure the strictness in the accumulator:
--
-- > flippedFoldl' f [] z = z
-- > flippedFoldl' f (x : xs) z = z `seq` flippedFoldl' f xs (f z x)
--
-- Rewriting to use lambdas, this is:
--
-- > flippedFoldl' f [] = \ b -> b
-- > flippedFoldl' f (x : xs) = \ b -> b `seq` r (f b x)
-- >     where r = flippedFoldl' f xs
--
-- The above has the form of a right fold, enabling a rewrite to:
--
-- > flippedFoldl' f = \ xs -> foldr f' id xs
-- >     where f' x r = \ b -> b `seq` r (f b x)
--
-- We can now unflip this to get 'foldl'':
--
-- > foldl' f z = \ xs -> foldr f' id xs z
-- >           -- \ xs -> flippedFoldl' f xs z
-- >   where f' x r = \ b -> b `seq` r (f b x)
--
-- The function __@foldr f' id xs@__ applied to @z@ is built corecursively, and
-- its terms are applied to an eagerly evaluated accumulator before further
-- terms are applied to the result.  As required, this runs in constant space,
-- and can be optimised to an efficient loop.
--
-- (The actual definition of 'foldl'' labels the lambdas in the definition of
-- __@f'@__ above as /oneShot/, which enables further optimisations).

--------------

-- $generative
--
-- #generative#
-- So far, we have not discussed /generative recursion/.  Unlike recursive
-- reduction or corecursion, instead of processing a sequence of elements
-- already in memory, generative recursion involves producing a possibly
-- unbounded sequence of values from an initial seed value.  The canonical
-- example of this is 'Data.List.unfoldr' for Lists, with variants available
-- for Vectors and various other structures.
--
-- A key issue with lists, when used generatively as /iterators/, rather than as
-- poor-man's containers (see [[1\]](#uselistsnot)), is that such iterators
-- tend to consume memory when used more than once.  A single traversal of a
-- list-as-iterator will run in constant space, but as soon as the list is
-- retained for reuse, its entire element sequence is stored in memory, and the
-- second traversal reads the copy, rather than regenerates the elements.  It
-- is sometimes better to recompute the elements rather than memoise the list.
--
-- Memoisation happens because the built-in Haskell list __@[]@__ is
-- represented as __data__, either empty or a /cons-cell/ holding the first
-- element and the tail of the list.  The @Foldable@ class enables a variant
-- representation of iterators as /functions/, which take an operator and a
-- starting accumulator and output a summary result.
--
-- The [@fmlist@](https://hackage.haskell.org/package/fmlist) package takes
-- this approach, by representing a list via its `foldMap` action.
--
-- Below we implement an analogous data structure using a representation
-- based on `foldr`.  This is an example of /Church encoding/
-- (named after Alonzo Church, inventor of the lambda calculus).
--
-- > {-# LANGUAGE RankNTypes #-}
-- > newtype FRList a = FR { unFR :: forall b. (a -> b -> b) -> b -> b }
--
-- The __@unFR@__ field of this type is essentially its `foldr` method
-- with the list as its first rather than last argument.  Thus we
-- immediately get a @Foldable@ instance (and a 'toList' function
-- mapping an __@FRList@__ to a regular list).
--
-- > instance Foldable FRList where
-- >     foldr f z l = unFR l f z
-- >     -- With older versions of @base@, also define sum, product, ...
-- >     -- to ensure use of the strict 'foldl''.
-- >     -- sum = foldl' (+) 0
-- >     -- ...
--
-- We can convert a regular list to an __@FRList@__ with:
--
-- > fromList :: [a] -> FRList a
-- > fromList as = FRList $ \ f z -> foldr f z as
--
-- However, reuse of an __@FRList@__ obtained in this way will typically
-- memoise the underlying element sequence.  Instead, we can define
-- __@FRList@__ terms directly:
--
-- > -- | Immediately return the initial accumulator
-- > nil :: FRList a
-- > nil = FRList $ \ _ z -> z
-- > {-# INLINE nil #-}
--
-- > -- | Fold the tail to use as an accumulator with the new initial element
-- > cons :: a -> FRList a -> FRList a
-- > cons a l = FRList $ \ f z -> f a (unFR l f z)
-- > {-# INLINE cons #-}
--
-- More crucially, we can also directly define the key building block for
-- generative recursion:
--
-- > -- | Generative recursion, dual to `foldr`.
-- > unfoldr :: (s -> Maybe (a, s)) -> s -> FRList a
-- > unfoldr g s0 = FR generate
-- >   where generate f z = loop s0
-- >           where loop s | Just (a, t) <- g s = f a (loop t)
-- >                        | otherwise = z
-- > {-# INLINE unfoldr #-}
--
-- Which can, for example, be specialised to number ranges:
--
-- > -- | Generate a range of consecutive integral values.
-- > range :: (Ord a, Integral a) => a -> a -> FRList a
-- > range lo hi =
-- >     unfoldr (\s -> if s > hi then Nothing else Just (s, s+1)) lo
-- > {-# INLINE range #-}
--
-- The program below, when compiled with optimisation:
--
-- > main :: IO ()
-- > main = do
-- >     let r :: FRList Int
-- >         r = range 1 10000000
-- >      in print (sum r, length r)
--
-- produces the expected output with no noticeable garbage-collection, despite
-- reuse of the __@FRList@__ term __@r@__.
--
-- > (50000005000000,10000000)
-- >     52,120 bytes allocated in the heap
-- >      3,320 bytes copied during GC
-- >     44,376 bytes maximum residency (1 sample(s))
-- >     25,256 bytes maximum slop
-- >          3 MiB total memory in use (0 MB lost due to fragmentation)
--
-- The Weak Head Normal Form of an __@FRList@__ is a lambda abstraction not a
-- data value, and reuse does not lead to memoisation.  Reuse of the iterator
-- above is somewhat contrived, when computing multiple folds over a common
-- list, you should generally traverse a  list only [once](#multipass).  The
-- goal is to demonstrate that the separate computations of the 'sum' and
-- 'length' run efficiently in constant space, despite reuse.  This would not
-- be the case with the list @[1..10000000]@.
--
-- This is, however, an artificially simple reduction.  More typically, there
-- are likely to be some allocations in the inner loop, but the temporary
-- storage used will be garbage-collected as needed, and overall memory
-- utilisation will remain modest and will not scale with the size of the list.
--
-- If we go back to built-in lists (i.e. __@[]@__), but avoid reuse by
-- performing reduction in a single pass, as below:
--
-- > data PairS a b = P !a !b -- We define a strict pair datatype
-- >
-- > main :: IO ()
-- > main = do
-- >     let l :: [Int]
-- >         l = [1..10000000]
-- >      in print $ average l
-- >   where
-- >     sumlen :: PairS Int Int -> Int -> PairS Int Int
-- >     sumlen (P s l) a = P (s + a) (l + 1)
-- >
-- >     average is =
-- >         let (P s l) = foldl' sumlen (P 0 0) is
-- >          in (fromIntegral s :: Double) / fromIntegral l
--
-- the result is again obtained in constant space:
--
-- > 5000000.5
-- >          102,176 bytes allocated in the heap
-- >            3,320 bytes copied during GC
-- >           44,376 bytes maximum residency (1 sample(s))
-- >           25,256 bytes maximum slop
-- >                3 MiB total memory in use (0 MB lost due to fragmentation)
--
-- (and, in fact, faster than with __@FRList@__ by a small factor).
--
-- The __@[]@__ list structure works as an efficient iterator when used
-- just once.  When space-leaks via list reuse are not a concern, and/or
-- memoisation is actually desirable, the regular list implementation is
-- likely to be faster.  This is not a suggestion to replace all your uses of
-- __@[]@__ with a generative alternative.
--
-- The __@FRList@__ type could be further extended with instances of 'Functor',
-- 'Applicative', 'Monad', 'Alternative', etc., and could then provide a
-- fully-featured list type, optimised for reuse without space-leaks.  If,
-- however, all that's required is space-efficient, re-use friendly iteration,
-- less is perhaps more, and just @Foldable@ may be sufficient.

--------------

-- $multipass
--
-- #multipass#
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
-- > data PairS a b = P !a !b -- We define a strict pair datatype
-- >
-- > -- | Compute sum and length in a single pass, then reduce to the average.
-- > average :: (Foldable f, Fractional a) => f a -> a
-- > average xs =
-- >     let sumlen (P s l) a = P (s + a) (l + 1 :: Int)
-- >         (P s l) = foldl' sumlen (P 0 0) xs
-- >      in s / fromIntegral l
--
-- The above example is somewhat contrived, some structures keep track of their
-- length internally, and can return it in /O(1)/ time, so this particular
-- recipe for averages is not always the most efficient.  In general, composite
-- aggregate functions of large structures benefit from single-pass reduction.
-- This is especially the case when reuse of a list and memoisation of its
-- elements is thereby avoided.

--------------

-- $laws
-- #laws#
--
-- The type constructor 'Endo' from "Data.Monoid", associates with each type
-- __@b@__ the __@newtype@__-encapsulated type of functions mapping __@b@__ to
-- itself.  Functions from a type to itself are called /endomorphisms/, hence
-- the name /Endo/.  The type __@Endo b@__ is a 'Monoid' under function
-- composition:
--
-- > newtype Endo b = Endo { appEndo :: b -> b }
-- > instance Semigroup Endo b where
-- >     Endo f <> Endo g = Endo (f . g)
-- > instance Monoid Endo b where
-- >     mempty = Endo id
--
-- For every 'Monoid' m, we also have a 'Dual' monoid __@Dual m@__ which
-- combines elements in the opposite order:
--
-- > newtype Dual m = Dual { getDual :: m }
-- > instance Semigroup m => Semigroup Dual m where
-- >     Dual a <> Dual b = Dual (b <> a)
-- > instance Monoid m => Monoid Dual m where
-- >     mempty = Dual mempty
--
-- With the above preliminaries out of the way, 'Foldable' instances are
-- expected to satisfy the following laws:
--
-- The 'foldr' method must be equivalent in value and strictness to replacing
-- each element __@a@__ of a 'Foldable' structure with __@Endo (f a)@__,
-- composing these via 'foldMap' and applying the result to the base case
-- __@z@__:
--
-- > foldr f z t = appEndo (foldMap (Endo . f) t ) z
--
-- Likewise, the 'foldl' method must be equivalent in value and strictness
-- to composing the functions __@flip f a@__ in reverse order and applying
-- the result to the base case:
--
-- > foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
--
-- When the elements of the structure are taken from a 'Monoid', the
-- definition of 'fold' must agree with __@foldMap id@__:
--
-- > fold = foldMap id
--
-- The 'length' method must agree with a 'foldMap' mapping each element to
-- __@Sum 1@__ (The 'Sum' type abstracts numbers as a monoid under addition).
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
-- > product = foldl' (*) 1
--
-- If the 'Foldable' structure has a 'Functor' instance, then for every
-- function __@f@__ mapping the elements into a 'Monoid', it should satisfy:
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
-- Since 'Foldable' does not have 'Functor' as a superclass, it is possible to
-- define 'Foldable' instances for structures that constrain their element
-- types.  Therefore, __@Set@__ can be 'Foldable', even though sets keep their
-- elements in ascending order.  This requires the elements to be comparable,
-- which precludes defining a 'Functor' instance for @Set@.
--
-- The 'Foldable' class makes it possible to use idioms familiar from the @List@
-- type with container structures that are better suited to the task at hand.
-- This supports use of more appropriate 'Foldable' data types, such as @Seq@,
-- @Set@, @NonEmpty@, etc., without requiring new idioms (see
-- [[1\]](#uselistsnot) for when not to use lists).
--
-- The more general methods of the 'Foldable' class are now exported by the
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

-- $linear
--
-- It is perhaps worth noting that since the __`elem`__ function in the
-- 'Foldable' class carries only an __`Eq`__ constraint on the element type,
-- search for the presence or absence of an element in the structure generally
-- takes /O(n)/ time, even for ordered structures like __@Set@__ that are
-- potentially capable of performing the search faster.  (The @member@ function
-- of the @Set@ module carries an `Ord` constraint, and can perform the search
-- in /O(log n)/ time).
--
-- An alternative to Foldable's __`elem`__ method is required in order to
-- abstract potentially faster than linear search over general container
-- structures.  This can be achieved by defining an additional type class (e.g.
-- @HasMember@ below).  Instances of such a type class (that are also
-- `Foldable') can employ the `elem` linear search as a last resort, when
-- faster search is not supported.
--
-- > {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- >
-- > import qualified Data.Set as Set
-- >
-- > class Eq a => HasMember t a where
-- >     member :: a -> t a -> Bool
-- >
-- > instance Eq a => HasMember [] a where
-- >     member = elem
-- > [...]
-- > instance Ord a => HasMember Set.Set a where
-- >     member = Set.member
--
-- The above suggests that 'elem' may be a misfit in the 'Foldable' class.
-- Alternative design ideas are solicited on GHC's bug tracker via issue
-- [\#20421](https://gitlab.haskell.org/ghc/ghc/-/issues/20421).
--
-- Note that some structure-specific optimisations may of course be possible
-- directly in the corresponding @Foldable@ instance, e.g. with @Set@ the size
-- of the set is known in advance, without iterating to count the elements, and
-- its `length` instance takes advantage of this to return the size directly.

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
--
--  * [3] \"A tutorial on the universality and expressiveness of fold\",
--    by Graham Hutton, J\. Functional Programming 9 (4): 355–372, July 1999,
--    online at <http://www.cs.nott.ac.uk/~pszgmh/fold.pdf>.
