{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Foldable
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

module GHC.Internal.Data.Foldable (
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

import GHC.Internal.Data.Bool
import GHC.Internal.Data.Either
import GHC.Internal.Data.Eq
import GHC.Internal.Data.Functor.Utils (Max(..), Min(..), (#.))
import qualified GHC.Internal.List as List
import GHC.Internal.Data.Maybe
import GHC.Internal.Data.Monoid
import GHC.Internal.Data.Ord
import GHC.Internal.Data.Proxy

import GHC.Internal.Arr  ( Array(..), elems, numElements,
                  foldlElems, foldrElems,
                  foldlElems', foldrElems',
                  foldl1Elems, foldr1Elems)
import GHC.Internal.Base hiding ( foldr )
import GHC.Internal.Generics
import GHC.Internal.Tuple (Solo (..))
import GHC.Internal.Num  ( Num(..) )

-- $setup
-- >>> :set -XDeriveFoldable
-- >>> import Prelude
-- >>> import GHC.Internal.Data.Monoid (Product (..), Sum (..))
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
    -- @since base-4.13.0.0
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
    -- @since base-4.6.0.0
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
    -- @since base-4.6.0.0
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
    -- @since base-4.8.0.0
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
    -- @since base-4.8.0.0
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
    -- @since base-4.8.0.0
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
    -- @since base-4.8.0.0
    elem :: Eq a => a -> t a -> Bool
    elem = any . (==)

    -- | The largest element of a non-empty structure. This function is
    -- equivalent to @'foldr1' 'max'@, and its behavior on structures with
    -- multiple largest elements depends on the relevant implementation of
    -- 'max'. For the default implementation of 'max' (@max x y = if x <= y
    -- then y else x@), structure order is used as a tie-breaker: if there are
    -- multiple largest elements, the rightmost of them is chosen (this is
    -- equivalent to @'maximumBy' 'compare'@).
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
    -- @since base-4.8.0.0
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
       getMax . foldMap' (Max #. (Just :: a -> Maybe a))
    {-# INLINEABLE maximum #-}

    -- | The least element of a non-empty structure. This function is
    -- equivalent to @'foldr1' 'min'@, and its behavior on structures with
    -- multiple largest elements depends on the relevant implementation of
    -- 'min'. For the default implementation of 'min' (@min x y = if x <= y
    -- then x else y@), structure order is used as a tie-breaker: if there are
    -- multiple least elements, the leftmost of them is chosen (this is
    -- equivalent to @'minimumBy' 'compare'@).
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
    -- @since base-4.8.0.0
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
    -- @since base-4.8.0.0
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
    -- @since base-4.8.0.0
    product :: Num a => t a -> a
    product = getProduct #. foldMap' Product
    {-# INLINEABLE product #-}

-- instances for Prelude types

-- | @since base-2.01
instance Foldable Maybe where
    foldMap = maybe mempty

    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

    foldl _ z Nothing = z
    foldl f z (Just x) = f z x

-- | @since base-2.01
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

-- | @since base-4.9.0.0
instance Foldable NonEmpty where
  foldr f z (a :| as) = f a (List.foldr f z as)
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

  foldMap f (a :| as) = f a `mappend` foldMap f as
  fold (m :| ms) = m `mappend` fold ms
  toList (a :| as) = a : as

-- | @since base-4.7.0.0
instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

    length (Left _)  = 0
    length (Right _) = 1

    null             = isLeft

-- | @since base-4.15
deriving instance Foldable Solo

-- | @since base-4.7.0.0
instance Foldable ((,) a) where
    foldMap f (_, y) = f y

    foldr f z (_, y) = f y z
    length _  = 1
    null _ = False

-- | @since base-4.8.0.0
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

-- | @since base-4.7.0.0
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

-- | @since base-4.8.0.0
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

-- | @since base-4.8.0.0
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

-- | @since base-4.8.0.0
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

-- | @since base-4.8.0.0
instance Foldable First where
    foldMap f = foldMap f . getFirst

-- | @since base-4.8.0.0
instance Foldable Last where
    foldMap f = foldMap f . getLast

-- | @since base-4.12.0.0
instance (Foldable f) => Foldable (Alt f) where
    foldMap f = foldMap f . getAlt

-- | @since base-4.12.0.0
instance (Foldable f) => Foldable (Ap f) where
    foldMap f = foldMap f . getAp

-- Instances for GHC.Generics
-- | @since base-4.9.0.0
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

-- | @since base-4.9.0.0
deriving instance Foldable V1

-- | @since base-4.9.0.0
deriving instance Foldable Par1

-- | @since base-4.9.0.0
deriving instance Foldable f => Foldable (Rec1 f)

-- | @since base-4.9.0.0
deriving instance Foldable (K1 i c)

-- | @since base-4.9.0.0
deriving instance Foldable f => Foldable (M1 i c f)

-- | @since base-4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)

-- | @since base-4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :*: g)

-- | @since base-4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :.: g)

-- | @since base-4.9.0.0
deriving instance Foldable UAddr

-- | @since base-4.9.0.0
deriving instance Foldable UChar

-- | @since base-4.9.0.0
deriving instance Foldable UDouble

-- | @since base-4.9.0.0
deriving instance Foldable UFloat

-- | @since base-4.9.0.0
deriving instance Foldable UInt

-- | @since base-4.9.0.0
deriving instance Foldable UWord

-- Instances for Data.Ord
-- | @since base-4.12.0.0
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

-- | The sum of a collection of actions using '(<|>)', generalizing 'concat'.
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

-- | The sum of a collection of actions using '(<|>)', generalizing 'concat'.
--
-- 'msum' is just like 'asum', but specialised to 'MonadPlus'.
--
-- ==== __Examples__
--
-- Basic usage, using the 'MonadPlus' instance for 'Maybe':
--
-- >>> msum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"
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
-- given comparison function. Structure order is used as a tie-breaker: if
-- there are multiple largest elements, the rightmost of them is chosen.
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
-- #22609 showed that maximumBy is too large to reliably inline,
-- See Note [maximumBy/minimumBy INLINE pragma]
{-# INLINE[2] maximumBy #-}

-- | The least element of a non-empty structure with respect to the
-- given comparison function. Structure order is used as a tie-breaker: if
-- there are multiple least elements, the leftmost of them is chosen.
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
-- See Note [maximumBy/minimumBy INLINE pragma]
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = fromMaybe (errorWithoutStackTrace "minimumBy: empty structure")
  . foldl' min' Nothing
  where
    min' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> y
        _ -> x
-- See Note [maximumBy/minimumBy INLINE pragma]
{-# INLINE[2] minimumBy #-}


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

Note [maximumBy/minimumBy INLINE pragma]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently maximumBy/minimumBy wrap the accumulator into Maybe to deal with the
empty case. Commonly one would just pass in a bottom default value alas this is
not easily done here if we want to remain strict in the accumulator.
See #17867 for why we want to be strict in the accumulator here.

For optimal code we want the Maybe to optimize away and the accumulator to be
unpacked if possible. For this to happen we need:
* SpecConstr to eliminate the Maybe
* W/W to unpack the accumulator
This only happens if we compile the RHS with -O2 at a specific type.
There are two ways to achieve this: Using a SPECIALIZE pragma inside base for a
blessed set of types since we know base will be compiled using -O2.
Or using INLINE and counting at call sites to be compiled with -O2.


We've chosen to use INLINE as this guarantees optimal code at -O2 no matter what
element type is used. However this comes at the cost of less optimal code when
the call site is using -O as SpecConstr won't fire, preventing W/W from firing
as well. See #22609 and the discussion in !9565.
Sadly we can't use both SPECIALIZE and INLINE. This would result in the RHS being
inlined before the specialization rule fires. Giving the same result as if we had
only used INLINE.
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
