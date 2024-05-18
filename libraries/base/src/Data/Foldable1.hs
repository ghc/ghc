-- |
-- Copyright: Edward Kmett, Oleg Grenrus
-- License: BSD-3-Clause
--
-- A class of non-empty data structures that can be folded to a summary value.
--
-- @since 4.18.0.0

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Foldable1 (
    Foldable1(..),
    foldr1, foldr1',
    foldl1, foldl1',
    intercalate1,
    foldrM1,
    foldlM1,
    foldrMapM1,
    foldlMapM1,
    maximumBy,
    minimumBy,
    ) where

import GHC.Internal.Data.Foldable      (Foldable, foldlM, foldr)
import GHC.Internal.Data.List          (foldl, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
       (Dual (..), First (..), Last (..), Max (..), Min (..), Product (..),
       Semigroup (..), Sum (..))
import GHC.Tuple (Solo (..))
import Prelude
       (Maybe (..), Monad (..), Ord, Ordering (..), id, seq, ($!), ($), (.),
       (=<<), flip, const, error)

import qualified Data.List.NonEmpty as NE

import Data.Complex (Complex (..))
import GHC.Generics
       (M1 (..), Par1 (..), Rec1 (..), V1, (:*:) (..), (:+:) (..), (:.:) (..))

import GHC.Internal.Data.Ord (Down (..))

import qualified GHC.Internal.Data.Monoid as Mon

-- Instances
import Data.Functor.Compose          (Compose (..))
import GHC.Internal.Data.Functor.Identity         (Identity (..))

import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum     as Functor

-- coerce
import GHC.Internal.Data.Coerce (Coercible, coerce)

-- $setup
-- >>> import Prelude hiding (foldr1, foldl1, head, last, minimum, maximum)
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import Data.Monoid (Sum(..))
-- >>> import Data.Functor.Identity

-------------------------------------------------------------------------------
-- Foldable1 type class
-------------------------------------------------------------------------------

-- | Non-empty data structures that can be folded.
--
-- @since 4.18.0.0
class Foldable t => Foldable1 t where
    {-# MINIMAL foldMap1 | foldrMap1 #-}

    -- At some point during design it was possible to define this class using
    -- only 'toNonEmpty'. But it seems a bad idea in general.
    --
    -- So currently we require either foldMap1 or foldrMap1
    --
    -- * foldMap1 defined using foldrMap1
    -- * foldrMap1 defined using foldMap1
    --
    -- One can always define an instance using the following pattern:
    --
    --     toNonEmpty = ...
    --     foldMap f     = foldMap f     . toNonEmpty
    --     foldrMap1 f g = foldrMap1 f g . toNonEmpty

    -- | Given a structure with elements whose type is a 'Semigroup', combine
    -- them via the semigroup's @('<>')@ operator. This fold is
    -- right-associative and lazy in the accumulator. When you need a strict
    -- left-associative fold, use 'foldMap1'' instead, with 'id' as the map.
    --
    -- @since 4.18.0.0
    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    -- | Map each element of the structure to a semigroup, and combine the
    -- results with @('<>')@. This fold is right-associative and lazy in the
    -- accumulator. For strict left-associative folds consider 'foldMap1''
    -- instead.
    --
    -- >>> foldMap1 (:[]) (1 :| [2, 3, 4])
    -- [1,2,3,4]
    --
    -- @since 4.18.0.0
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m
    foldMap1 f = foldrMap1 f (\a m -> f a <> m)

    -- | A left-associative variant of 'foldMap1' that is strict in the
    -- accumulator. Use this for strict reduction when partial results are
    -- merged via @('<>')@.
    --
    -- >>> foldMap1' Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    -- @since 4.18.0.0
    foldMap1' :: Semigroup m => (a -> m) -> t a -> m
    foldMap1' f = foldlMap1' f (\m a -> m <> f a)

    -- | 'NonEmpty' list of elements of a structure, from left to right.
    --
    -- >>> toNonEmpty (Identity 2)
    -- 2 :| []
    --
    -- @since 4.18.0.0
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . foldMap1 singleton

    -- | The largest element of a non-empty structure.
    --
    -- >>> maximum (32 :| [64, 8, 128, 16])
    -- 128
    --
    -- @since 4.18.0.0
    maximum :: Ord a => t a -> a
    maximum = getMax #. foldMap1' Max

    -- | The least element of a non-empty structure.
    --
    -- >>> minimum (32 :| [64, 8, 128, 16])
    -- 8
    --
    -- @since 4.18.0.0
    minimum :: Ord a => t a -> a
    minimum = getMin #. foldMap1' Min

    -- | The first element of a non-empty structure.
    --
    -- >>> head (1 :| [2, 3, 4])
    -- 1
    --
    -- @since 4.18.0.0
    head :: t a -> a
    head = getFirst #. foldMap1 First

    -- | The last element of a non-empty structure.
    --
    -- >>> last (1 :| [2, 3, 4])
    -- 4
    --
    -- @since 4.18.0.0
    last :: t a -> a
    last = getLast #. foldMap1 Last

    -- | Right-associative fold of a structure, lazy in the accumulator.
    --
    -- In case of 'NonEmpty' lists, 'foldrMap1', when given a function @f@, a
    -- binary operator @g@, and a list, reduces the list using @g@ from right to
    -- left applying @f@ to the rightmost element:
    --
    -- > foldrMap1 f g (x1 :| [x2, ..., xn1, xn]) == x1 `g` (x2 `g` ... (xn1 `g` (f xn))...)
    --
    -- Note that since the head of the resulting expression is produced by
    -- an application of @g@ to the first element of the list, if @g@ is lazy
    -- in its right argument, 'foldrMap1' can produce a terminating expression
    -- from an unbounded list.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to:
    --
    -- @foldrMap1 f g = foldrMap1 f g . 'toNonEmpty'@
    --
    -- @since 4.18.0.0
    foldrMap1 :: (a -> b) -> (a -> b -> b) -> t a -> b
    foldrMap1 f g xs =
        appFromMaybe (foldMap1 (FromMaybe #. h) xs) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g a b

    -- | Left-associative fold of a structure but with strict application of the
    -- operator.
    --
    -- This ensures that each step of the fold is forced to Weak Head Normal
    -- Form before being applied, avoiding the collection of thunks that would
    -- otherwise occur. This is often what you want to strictly reduce a
    -- finite structure to a single strict result.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to:
    --
    -- @foldlMap1' f z = foldlMap1' f z . 'toNonEmpty'@
    --
    -- @since 4.18.0.0
    foldlMap1' :: (a -> b) -> (b -> a -> b) -> t a -> b
    foldlMap1' f g xs =
        foldrMap1 f' g' xs SNothing
      where
        -- f' :: a -> SMaybe b -> b
        f' a SNothing  = f a
        f' a (SJust b) = g b a

        -- g' :: a -> (SMaybe b -> b) -> SMaybe b -> b
        g' a x SNothing  = x $! SJust (f a)
        g' a x (SJust b) = x $! SJust (g b a)

    -- | Left-associative fold of a structure, lazy in the accumulator.  This is
    -- rarely what you want, but can work well for structures with efficient
    -- right-to-left sequencing and an operator that is lazy in its left
    -- argument.
    --
    -- In case of 'NonEmpty' lists, 'foldlMap1', when given a function @f@, a
    -- binary operator @g@, and a list, reduces the list using @g@ from left to
    -- right applying @f@ to the leftmost element:
    --
    -- > foldlMap1 f g (x1 :| [x2, ..., xn]) == (...(((f x1) `g` x2) `g`...) `g` xn
    --
    -- Note that to produce the outermost application of the operator the entire
    -- input list must be traversed. This means that 'foldlMap1' will diverge if
    -- given an infinite list.
    --
    -- If you want an efficient strict left-fold, you probably want to use
    -- 'foldlMap1''  instead of 'foldlMap1'. The reason for this is that the
    -- latter does not force the /inner/ results (e.g. @(f x1) \`g\` x2@ in the
    -- above example) before applying them to the operator (e.g. to
    -- @(\`g\` x3)@). This results in a thunk chain \(O(n)\) elements long,
    -- which then must be evaluated from the outside-in.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to:
    --
    -- @foldlMap1 f g = foldlMap1 f g . 'toNonEmpty'@
    --
    -- @since 4.18.0.0
    foldlMap1 :: (a -> b) -> (b -> a -> b) -> t a -> b
    foldlMap1 f g xs =
        appFromMaybe (getDual (foldMap1 ((Dual . FromMaybe) #. h) xs)) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g b a

    -- | 'foldrMap1'' is a variant of 'foldrMap1' that performs strict reduction
    -- from right to left, i.e. starting with the right-most element. The input
    -- structure /must/ be finite, otherwise 'foldrMap1'' runs out of space
    -- (/diverges/).
    --
    -- If you want a strict right fold in constant space, you need a structure
    -- that supports faster than \(O(n)\) access to the right-most element.
    --
    -- This method does not run in constant space for structures such as
    -- 'NonEmpty' lists that don't support efficient right-to-left iteration and
    -- so require \(O(n)\) space to perform right-to-left reduction. Use of this
    -- method with such a structure is a hint that the chosen structure may be a
    -- poor fit for the task at hand. If the order in which the elements are
    -- combined is not important, use 'foldlMap1'' instead.
    --
    -- @since 4.18.0.0
    foldrMap1' :: (a -> b) -> (a -> b -> b) -> t a -> b
    foldrMap1' f g xs =
        foldlMap1 f' g' xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g a b

        g' bb a SNothing  = bb $! SJust (f a)
        g' bb a (SJust b) = bb $! SJust (g a b)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | A variant of 'foldrMap1' where the rightmost element maps to itself.
--
-- @since 4.18.0.0
foldr1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldr1 = foldrMap1 id
{-# INLINE foldr1 #-}

-- | A variant of 'foldrMap1'' where the rightmost element maps to itself.
--
-- @since 4.18.0.0
foldr1' :: Foldable1 t => (a -> a -> a) -> t a -> a
foldr1' = foldrMap1' id
{-# INLINE foldr1' #-}

-- | A variant of 'foldlMap1' where the leftmost element maps to itself.
--
-- @since 4.18.0.0
foldl1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldl1 = foldlMap1 id
{-# INLINE foldl1 #-}

-- | A variant of 'foldlMap1'' where the leftmost element maps to itself.
--
-- @since 4.18.0.0
foldl1' :: Foldable1 t => (a -> a -> a) -> t a -> a
foldl1' = foldlMap1' id
{-# INLINE foldl1' #-}

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
-- @since 4.18.0.0
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 = flip intercalateMap1 id

intercalateMap1 :: (Foldable1 t, Semigroup m) => m -> (a -> m) -> t a -> m
intercalateMap1 j f = flip joinee j . foldMap1 (JoinWith . const . f)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
--
-- @since 4.18.0.0
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 = foldrMapM1 return

-- | Map variant of 'foldrM1'.
--
-- @since 4.18.0.0
foldrMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
foldrMapM1 g f = go . toNonEmpty
  where
    go (e:|es) =
      case es of
        []   -> g e
        x:xs -> f e =<< go (x:|xs)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
--
-- @since 4.18.0.0
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 = foldlMapM1 return

-- | Map variant of 'foldlM1'.
--
-- @since 4.18.0.0
foldlMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
foldlMapM1 g f t = g x >>= \y -> foldlM f y xs
  where x:|xs = toNonEmpty t

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
--
-- @since 4.18.0.0
maximumBy :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1' max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
--
-- @since 4.18.0.0
minimumBy :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1' min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

-- | Used for default toNonEmpty implementation.
newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  xs <> ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL #. (:|)

-- | Convert a dlist to a non-empty list
runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($ []) . unNEDL
{-# INLINE runNonEmptyDList #-}

-- | Used for foldrMap1 and foldlMap1 definitions
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- | Strict maybe, used to implement default foldlMap1' etc.
data SMaybe a = SNothing | SJust !a

-- | Used to implement intercalate1/Map
newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-------------------------------------------------------------------------------
-- Instances for misc base types
-------------------------------------------------------------------------------

-- | @since 4.18.0.0
instance Foldable1 NonEmpty where
    foldMap1 f (x :| xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

    foldMap1' f (x :| xs) = foldl' (\m y -> m <> f y) (f x) xs

    toNonEmpty = id

    foldrMap1 g f (x :| xs) = go x xs where
        go y [] = g y
        go y (z : zs) = f y (go z zs)

    foldlMap1  g f (x :| xs) = foldl f (g x) xs
    foldlMap1' g f (x :| xs) = let gx = g x in gx `seq` foldl' f gx xs

    head = NE.head
    last = NE.last

-- | @since 4.18.0.0
instance Foldable1 Down where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Complex where
    foldMap1 f (x :+ y) = f x <> f y

    toNonEmpty (x :+ y) = x :| y : []

-------------------------------------------------------------------------------
-- Instances for tuples
-------------------------------------------------------------------------------

-- 3+ tuples are not Foldable/Traversable

-- | @since 4.18.0.0
instance Foldable1 Solo where
    foldMap1 f (MkSolo y) = f y
    toNonEmpty (MkSolo x) = x :| []
    minimum (MkSolo x) = x
    maximum (MkSolo x) = x
    head (MkSolo x) = x
    last (MkSolo x) = x

-- | @since 4.18.0.0
instance Foldable1 ((,) a) where
    foldMap1 f (_, y) = f y
    toNonEmpty (_, x) = x :| []
    minimum (_, x) = x
    maximum (_, x) = x
    head (_, x) = x
    last (_, x) = x

-------------------------------------------------------------------------------
-- Monoid / Semigroup instances
-------------------------------------------------------------------------------

-- | @since 4.18.0.0
instance Foldable1 Dual where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Sum where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Product where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Min where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Max where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 First where
    foldMap1 = coerce

-- | @since 4.18.0.0
instance Foldable1 Last where
    foldMap1 = coerce

-- | @since 4.18.0.0
deriving instance (Foldable1 f) => Foldable1 (Mon.Alt f)

-- | @since 4.18.0.0
deriving instance (Foldable1 f) => Foldable1 (Mon.Ap f)

-------------------------------------------------------------------------------
-- GHC.Generics instances
-------------------------------------------------------------------------------

-- | @since 4.18.0.0
instance Foldable1 V1 where
    foldMap1 _ x = x `seq` error "foldMap1 @V1"

-- | @since 4.18.0.0
instance Foldable1 Par1 where
    foldMap1 = coerce

-- | @since 4.18.0.0
deriving instance Foldable1 f => Foldable1 (Rec1 f)

-- | @since 4.18.0.0
deriving instance Foldable1 f => Foldable1 (M1 i c f)

-- | @since 4.18.0.0
instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
    foldMap1 f (L1 x) = foldMap1 f x
    foldMap1 f (R1 y) = foldMap1 f y

-- | @since 4.18.0.0
instance (Foldable1 f, Foldable1 g) => Foldable1 (f :*: g) where
    foldMap1 f (x :*: y) = foldMap1 f x <> foldMap1 f y

-- | @since 4.18.0.0
instance (Foldable1 f, Foldable1 g) => Foldable1 (f :.: g) where
    foldMap1 f = foldMap1 (foldMap1 f) . unComp1

-------------------------------------------------------------------------------
-- Extra instances
-------------------------------------------------------------------------------

-- | @since 4.18.0.0
instance Foldable1 Identity where
    foldMap1      = coerce

    foldrMap1  g _ = coerce g
    foldrMap1' g _ = coerce g
    foldlMap1  g _ = coerce g
    foldlMap1' g _ = coerce g

    toNonEmpty (Identity x) = x :| []

    last    = coerce
    head    = coerce
    minimum = coerce
    maximum = coerce

-- | It would be enough for either half of a product to be 'Foldable1'.
-- Other could be 'Foldable'.
instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
    foldMap1 f (Functor.Pair x y)    = foldMap1 f x <> foldMap1 f y
    foldrMap1 g f (Functor.Pair x y) = foldr f (foldrMap1 g f y) x

    head (Functor.Pair x _) = head x
    last (Functor.Pair _ y) = last y

-- | @since 4.18.0.0
instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
    foldMap1 f (Functor.InL x) = foldMap1 f x
    foldMap1 f (Functor.InR y) = foldMap1 f y

    foldrMap1 g f (Functor.InL x) = foldrMap1 g f x
    foldrMap1 g f (Functor.InR y) = foldrMap1 g f y

    toNonEmpty (Functor.InL x) = toNonEmpty x
    toNonEmpty (Functor.InR y) = toNonEmpty y

    head (Functor.InL x) = head x
    head (Functor.InR y) = head y
    last (Functor.InL x) = last x
    last (Functor.InR y) = last y

    minimum (Functor.InL x) = minimum x
    minimum (Functor.InR y) = minimum y
    maximum (Functor.InL x) = maximum x
    maximum (Functor.InR y) = maximum y

-- | @since 4.18.0.0
instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose

    foldrMap1 f g = foldrMap1 (foldrMap1 f g) (\xs x -> foldr g x xs) . getCompose

    head = head . head . getCompose
    last = last . last . getCompose

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
