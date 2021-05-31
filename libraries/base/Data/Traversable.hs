{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.  Instances are expected to satisfy
-- the listed [laws](#laws).
-----------------------------------------------------------------------------

module Data.Traversable (
    -- * The 'Traversable' class
    Traversable(..),
    -- * Utility functions
    for,
    forM,
    mapAccumL,
    mapAccumR,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,

    -- * Overview
    -- $overview

    -- ** The 'traverse' and 'mapM' methods
    -- $traverse

    -- ** Validation use-case
    -- $validation

    -- ** The 'sequenceA' and 'sequence' methods
    -- $sequence

    -- ** Example binary tree instance
    -- $tree_instance

    -- ** Construction
    --
    -- $construction

    -- * Laws
    --
    -- $laws

    -- * See also
    -- $also
    ) where

-- It is convenient to use 'Const' here but this means we must
-- define a few instances here which really belong in Control.Applicative
import Control.Applicative ( Const(..), ZipList(..) )
import Data.Coerce
import Data.Either ( Either(..) )
import Data.Foldable
import Data.Functor
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Utils ( StateL(..), StateR(..) )
import Data.Monoid ( Dual(..), Sum(..), Product(..),
                     First(..), Last(..), Alt(..), Ap(..) )
import Data.Ord ( Down(..) )
import Data.Proxy ( Proxy(..) )

import GHC.Arr
import GHC.Base ( Applicative(..), Monad(..), Monoid, Maybe(..), NonEmpty(..),
                  ($), (.), id, flip )
import GHC.Generics
import qualified GHC.List as List ( foldr )
import GHC.Tuple (Solo (..))

-- $setup
-- >>> import Prelude
-- >>> import Data.Maybe (catMaybes, mapMaybe)
-- >>> import Data.Either (rights)
-- >>> import Data.Foldable (traverse_)

-- XXX: Missing haddock feature.  Links to anchors in other modules
-- don't have a sensible way to name the link within the module itself.
-- Thus, the below "Data.Traversable#overview" works well when shown as
-- @Data.Traversable@ from other modules, but in the home module it should
-- be possible to specify alternative link text. :-(

-- | Functors representing data structures that can be traversed from
-- left to right, performing an action on each element.
--
-- A more detailed description can be found in the overview section of
-- "Data.Traversable#overview".
--
class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequenceA #-}

    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- In the first two examples we show each evaluated action mapping to the
    -- output structure.
    --
    -- >>> traverse Just [1,2,3,4]
    -- Just [1,2,3,4]
    --
    -- >>> traverse id [Right 1, Right 2, Right 3, Right 4]
    -- Right [1,2,3,4]
    --
    -- In the next examples, we show that 'Nothing' and 'Left' values short
    -- circuit the created structure.
    --
    -- >>> traverse (const Nothing) [1,2,3,4]
    -- Nothing
    --
    -- >>> traverse (\x -> if odd x then Just x else Nothing)  [1,2,3,4]
    -- Nothing
    --
    -- >>> traverse id [Right 1, Right 2, Right 3, Right 4, Left 0]
    -- Left 0
    --
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    {-# INLINE traverse #-}  -- See Note [Inline default methods]
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right, and
    -- collect the results. For a version that ignores the results
    -- see 'Data.Foldable.sequenceA_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- For the first two examples we show sequenceA fully evaluating a
    -- a structure and collecting the results.
    --
    -- >>> sequenceA [Just 1, Just 2, Just 3]
    -- Just [1,2,3]
    --
    -- >>> sequenceA [Right 1, Right 2, Right 3]
    -- Right [1,2,3]
    --
    -- The next two example show 'Nothing' and 'Just' will short circuit
    -- the resulting structure if present in the input. For more context,
    -- check the 'Traversable' instances for 'Either' and 'Maybe'.
    --
    -- >>> sequenceA [Just 1, Just 2, Just 3, Nothing]
    -- Nothing
    --
    -- >>> sequenceA [Right 1, Right 2, Right 3, Left 4]
    -- Left 4
    --
    sequenceA :: Applicative f => t (f a) -> f (t a)
    {-# INLINE sequenceA #-}  -- See Note [Inline default methods]
    sequenceA = traverse id

    -- | Map each element of a structure to a monadic action, evaluate
    -- these actions from left to right, and collect the results. For
    -- a version that ignores the results see 'Data.Foldable.mapM_'.
    --
    -- ==== __Examples__
    --
    -- 'mapM' is literally a 'traverse' with a type signature restricted
    -- to 'Monad'. Its implementation may be more efficient due to additional
    -- power of 'Monad'.
    --
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    {-# INLINE mapM #-}  -- See Note [Inline default methods]
    mapM = traverse

    -- | Evaluate each monadic action in the structure from left to
    -- right, and collect the results. For a version that ignores the
    -- results see 'Data.Foldable.sequence_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- The first two examples are instances where the input and
    -- and output of 'sequence' are isomorphic.
    --
    -- >>> sequence $ Right [1,2,3,4]
    -- [Right 1,Right 2,Right 3,Right 4]
    --
    -- >>> sequence $ [Right 1,Right 2,Right 3,Right 4]
    -- Right [1,2,3,4]
    --
    -- The following examples demonstrate short circuit behavior
    -- for 'sequence'.
    --
    -- >>> sequence $ Left [1,2,3,4]
    -- Left [1,2,3,4]
    --
    -- >>> sequence $ [Left 0, Right 1,Right 2,Right 3,Right 4]
    -- Left 0
    --
    sequence :: Monad m => t (m a) -> m (t a)
    {-# INLINE sequence #-}  -- See Note [Inline default methods]
    sequence = sequenceA

{- Note [Inline default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       mapM = traverse   -- Default method

   instance Traversable [] where
       {-# INLINE traverse #-}
       traverse = ...code for traverse on lists ...

This gives rise to a list-instance of mapM looking like this

  $fTraversable[]_$ctraverse = ...code for traverse on lists...
       {-# INLINE $fTraversable[]_$ctraverse #-}
  $fTraversable[]_$cmapM    = $fTraversable[]_$ctraverse

Now the $ctraverse obediently inlines into the RHS of $cmapM, /but/
that's all!  We get

  $fTraversable[]_$cmapM = ...code for traverse on lists...

with NO INLINE pragma!  This happens even though 'traverse' had an
INLINE pragma because the author knew it should be inlined pretty
vigorously.

Indeed, it turned out that the rhs of $cmapM was just too big to
inline, so all uses of mapM on lists used a terribly inefficient
dictionary-passing style, because of its 'Monad m =>' type.  Disaster!

Solution: add an INLINE pragma on the default method:

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       {-# INLINE mapM #-}     -- VERY IMPORTANT!
       mapM = traverse
-}

-- instances for Prelude types

-- | @since 2.01
instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

-- | @since 2.01
instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

-- | @since 4.9.0.0
instance Traversable NonEmpty where
  traverse f ~(a :| as) = liftA2 (:|) (f a) (traverse f as)

-- | @since 4.7.0.0
instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

-- | @since 4.15
deriving instance Traversable Solo

-- | @since 4.7.0.0
instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

-- | @since 2.01
instance Ix i => Traversable (Array i) where
    traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

-- | @since 4.7.0.0
instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = pure Proxy
    {-# INLINE mapM #-}
    sequence _ = pure Proxy
    {-# INLINE sequence #-}

-- | @since 4.7.0.0
instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

-- | @since 4.8.0.0
instance Traversable Dual where
    traverse f (Dual x) = Dual <$> f x

-- | @since 4.8.0.0
instance Traversable Sum where
    traverse f (Sum x) = Sum <$> f x

-- | @since 4.8.0.0
instance Traversable Product where
    traverse f (Product x) = Product <$> f x

-- | @since 4.8.0.0
instance Traversable First where
    traverse f (First x) = First <$> traverse f x

-- | @since 4.8.0.0
instance Traversable Last where
    traverse f (Last x) = Last <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Alt f) where
    traverse f (Alt x) = Alt <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Ap f) where
    traverse f (Ap x) = Ap <$> traverse f x

-- | @since 4.9.0.0
instance Traversable ZipList where
    traverse f (ZipList x) = ZipList <$> traverse f x

-- | @since 4.9.0.0
deriving instance Traversable Identity


-- Instances for GHC.Generics
-- | @since 4.9.0.0
instance Traversable U1 where
    traverse _ _ = pure U1
    {-# INLINE traverse #-}
    sequenceA _ = pure U1
    {-# INLINE sequenceA #-}
    mapM _ _ = pure U1
    {-# INLINE mapM #-}
    sequence _ = pure U1
    {-# INLINE sequence #-}

-- | @since 4.9.0.0
deriving instance Traversable V1

-- | @since 4.9.0.0
deriving instance Traversable Par1

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (Rec1 f)

-- | @since 4.9.0.0
deriving instance Traversable (K1 i c)

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (M1 i c f)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)

-- | @since 4.9.0.0
deriving instance Traversable UAddr

-- | @since 4.9.0.0
deriving instance Traversable UChar

-- | @since 4.9.0.0
deriving instance Traversable UDouble

-- | @since 4.9.0.0
deriving instance Traversable UFloat

-- | @since 4.9.0.0
deriving instance Traversable UInt

-- | @since 4.9.0.0
deriving instance Traversable UWord

-- Instance for Data.Ord
-- | @since 4.12.0.0
deriving instance Traversable Down

-- general functions

-- | 'for' is 'traverse' with its arguments flipped. For a version
-- that ignores the results see 'Data.Foldable.for_'.
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE for #-}
for = flip traverse

-- | 'forM' is 'mapM' with its arguments flipped. For a version that
-- ignores the results see 'Data.Foldable.forM_'.
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
{-# INLINE forM #-}
forM = flip mapM

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> mapAccumL (\a b -> (a + b, a)) 0 [1..10]
-- (55,[0,1,3,6,10,15,21,28,36,45])
--
-- >>> mapAccumL (\a b -> (a <> show b, a)) "0" [1..5]
-- ("012345",["0","01","012","0123","01234"])
--
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s

-- |The 'mapAccumR' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldr'; it applies a function to each element of a structure,
-- passing an accumulating parameter from right to left, and returning
-- a final value of this accumulator together with the new structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> mapAccumR (\a b -> (a + b, a)) 0 [1..10]
-- (55,[54,52,49,45,40,34,27,19,10,0])
--
-- >>> mapAccumR (\a b -> (a <> show b, a)) "0" [1..5]
-- ("054321",["05432","0543","054","05","0"])
--
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'traverse' is defined. (Using
--   `fmapDefault` with a `Traversable` instance defined only by
--   'sequenceA' will result in infinite recursion.)
--
-- @
-- 'fmapDefault' f ≡ 'runIdentity' . 'traverse' ('Identity' . f)
-- @
fmapDefault :: forall t a b . Traversable t
            => (a -> b) -> t a -> t b
{-# INLINE fmapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
fmapDefault = coerce (traverse :: (a -> Identity b) -> t a -> Identity (t b))

-- | This function may be used as a value for `Data.Foldable.foldMap`
-- in a `Foldable` instance.
--
-- @
-- 'foldMapDefault' f ≡ 'getConst' . 'traverse' ('Const' . f)
-- @
foldMapDefault :: forall t m a . (Traversable t, Monoid m)
               => (a -> m) -> t a -> m
{-# INLINE foldMapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
foldMapDefault = coerce (traverse :: (a -> Const m ()) -> t a -> Const m (t ()))

------------------

-- $overview
--
-- #overview#
-- Traversable structures can be thought of as polymorphic containers that
-- support element-wise sequencing of 'Applicative' (or 'Monad') effects whose
-- return values fill in new structures of __the same shape__ as the input.
-- Instances must satisfy the laws listed in the [Laws section](#laws).
--
-- The sequencing of effects in @Traversable@ structures is rather
-- straightforward, see the [Construction](#construction) section for details.
-- The diverse uses of @Traversable@ structures result from the many possible
-- choices of Applicative effects to thread through a traversable structure.
--
-- Every @Traversable@ structure is both a 'Functor' and 'Foldable' because it
-- is possible to implement both 'fmap' and 'foldMap' (also 'foldr', ...) in
-- terms of 'traverse'.
--
-- When there's no compelling reason to directly implement the @Functor@ and/or
-- @Foldable@ methods, one can use either or both of 'fmapDefault' and
-- 'foldMapDefault' to define the required instances.  It is then sufficient to
-- implement just 'traverse'.  Direct implementation of fine-tuned superclass
-- methods may of course produce more efficient code.

------------------

-- $traverse
-- For an 'Applicative' functor __@f@__ and a @Traversable@ functor __@t@__,
-- the type signatures of 'traverse' and 'fmap' are rather similar:
--
-- > fmap     :: (a -> f b) -> t a -> t (f b)
-- > traverse :: (a -> f b) -> t a -> f (t b)
--
-- The key difference is that 'fmap' produces a structure whose elements (of
-- type __@f b@__) are as-yet unevaluated effects, while 'traverse' produces an
-- aggregate effect yielding zero or more structures of type __@t b@__.  If any
-- such structures are produced, they all have /the same shape/ (list length,
-- graph of tree nodes, ...) as the input structure __@t a@__, but the slots
-- previously occupied by elements of type __@a@__ now hold elements of type
-- __@b@__.
--
-- For example, when __@f@__ is the __@IO@__ monad, and __@t@__ is __@List@__,
-- while 'fmap' yields a list of pending IO actions 'traverse' constructs an IO
-- action that evaluates to a list of the return values of the individual
-- actions performed left-to-right.
--
-- > traverse :: (a -> IO b) -> [a] -> IO [b]
--
-- The specialisation of 'traverse' to the case when __@f@__ is a 'Monad' is
-- called 'mapM', and is the more idiomatic choice in this case.  The two are
-- otherwise generally identical (though 'mapM' may be specifically optimised
-- for monads, and could be more efficient than using the more general
-- 'traverse').
--
-- > traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- > mapM     :: (Monad       m, Traversable t) => (a -> m b) -> t a -> m (t b)
--
-- Their flipped versions are 'for' and 'forM', respectively.
--
-- Let __@g :: a -> f b@__ and __@ts :: t a@__ be the arguments of 'traverse'
-- (or equivalently 'mapM').  Given the 'Foldable' superclass, we can use its
-- 'null' method to determine whether __@ts@__ is empty.  When __@ts@__ is
-- empty, the return value of 'traverse' is __@pure ts@__, regardless of the
-- function __@g@__.  There are no available values of type __@a@__ to __@g@__
-- can be applied; the only available means to construct the result is to use
-- the 'Applicative' functor __@f@__ from the type signature of __@g@__ to
-- locate the appropriate instance of 'pure':
--
-- prop> null ts ==> traverse g ts == pure ts
--
-- Otherwise, provided that __@ts@__ is non-empty and at least one value of
-- type __@t b@__ can be extracted from the return value of 'traverse', all
-- such values will have exactly /the same shape/ as the input __@t a@__.  The
-- 'traverse' function does not perform selective filtering of slots in the
-- output structure as with e.g. 'Data.Maybe.mapMaybe'.
--
-- >>> let incOdd n = if odd n then Just $ n + 1 else Nothing
-- >>> mapMaybe incOdd [1, 2, 3]
-- [2,4]
-- >>> traverse incOdd [1, 3, 5]
-- Just [2,4,6]
-- >>> traverse incOdd [1, 2, 3]
-- Nothing
--
-- In the above examples, with 'Maybe' as the 'Applicative' __@f@__, we see
-- that the number of __@t b@__ structures produced by 'traverse' may differ
-- from one: it is zero when the result short-circuits to __@Nothing@__.  The
-- same can happen when __@f@__ is __@List@__ and the result is __@[]@__, or
-- __@f@__ is __@Either e@__ and the result is __@Left (x :: e)@__, or perhaps
-- the 'Control.Applicative.empty' value of some
-- 'Control.Applicative.Alternative' functor.
--
-- More than one __@t b@__ structure may result when __@f b@__ can hold more
-- than one value of type __@b@__.  When __@f@__ is e.g. __@List@__, and the
-- map __@g :: a -> [b]@__ returns more than one value for some inputs __@a@__
-- (and at least one for all __@a@__), the result of __@mapM g ts@__ will
-- contain multiple structures of the same shape as __@ts@__:
--
-- prop> length (mapM g ts) == product (fmap (length . g) ts)
--
-- For example:
--
-- >>> length $ mapM (\n -> [1..n]) [1..6]
-- 720
-- >>> product $ length . (\n -> [1..n]) <$> [1..6]
-- 720
--
-- In other words, a traversal with a function __@g :: a -> [b]@__, over an
-- input structure __@t a@__, yields a list __@[t b]@__, whose length is the
-- product of the lengths of the lists that @g@ returns for each element of the
-- input structure!  The individual elements __@a@__ of the structure are
-- replaced by each element of __@g a@__ in turn:
--
-- >>> mapM (\n -> [1..n]) $ Just 3
-- [Just 1,Just 2,Just 3]
-- >>> mapM (\n -> [1..n]) [1..3]
-- [[1,1,1],[1,1,2],[1,1,3],[1,2,1],[1,2,2],[1,2,3]]
--
-- If any element of the structure __@t a@__ is mapped by @g@ to an empty list,
-- then the entire aggregate result is empty, because no value is available to
-- fill one of the slots of the output structure:
--
-- >>> mapM (\n -> [1..n]) $ [0..6] -- [1..0] is empty
-- []

------------------

-- $validation
--
-- #validation#
-- A hypothetical application of the above is to validate a structure:
--
-- >>> :{
-- validate :: Int -> Either (String, Int) Int
-- validate i = if odd i then Left ("That's odd", i) else Right i
-- :}
--
-- >>> traverse validate [2,4,6,8,10]
-- Right [2,4,6,8,10]
-- >>> traverse validate [2,4,6,8,9]
-- Left ("That's odd",9)
--
-- Since 'Nothing' is an empty structure, none of its elements are odd.
--
-- >>> traverse validate Nothing
-- Right Nothing
-- >>> traverse validate (Just 42)
-- Right (Just 42)
-- >>> traverse validate (Just 17)
-- Left ("That's odd",17)
--
-- However, this is not terribly efficient, because we pay the cost of
-- reconstructing the entire structure as a side effect of validation.
-- It is generally cheaper to just check all the elements and then use
-- the original structure if it is valid.  This can be done via the
-- methods of the 'Foldable' superclass, which perform only the
-- side effects without generating a new structure:
--
-- >>> traverse_ validate [2,4,6,8,10]
-- Right ()
-- >>> traverse_ validate [2,4,6,8,9]
-- Left ("That's odd",9)
--
-- The 'Foldable' instance should be defined in a manner that avoids
-- construction of an unnecessary copy of the container.
--
-- The @Foldable@ method 'mapM_' and its flipped version 'forM_' can be used
-- to sequence IO actions over all the elements of a @Traversable@ container
-- (just for their side-effects, ignoring any results) .  One special
-- case is a 'Maybe' container that optionally holds a value. Given:
--
-- > action :: a -> IO ()
-- > mvalue :: Maybe a
--
-- if you want to evaluate the __@action@__ in the @Just@ case, and do
-- nothing otherwise, you can write the more concise and more general:
--
-- > mapM_ action mvalue
--
-- rather than
--
-- > maybe (return ()) action mvalue
--
-- The 'mapM_' form works verbatim if the type of __@mvalue@__ is later
-- refactored from __@Maybe a@__ to __@Either e a@__ (assuming it remains
-- OK to silently do nothing in the error case).
--
-- There's even a generic way to handle empty values ('Nothing', 'Left', etc.):
--
-- > case traverse_ (const Nothing) mvalue of
-- >     Nothing -> mapM_ action mvalue -- mvalue is non-empty
-- >     Just () -> ... handle empty mvalue ...

------------------

-- $sequence
-- The 'sequenceA' and 'sequence' methods are useful when what you have is a
-- container of pending applicative or monadic effects, and you want to combine
-- them into a single effect that produces zero or more containers with the
-- computed values.
--
-- > sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- > sequence  :: (Monad       m, Traversable t) => t (m a) -> m (t a)
-- > sequenceA = traverse id -- default definition
-- > sequence  = sequenceA   -- default definition
--
-- When the monad __@m@__ is 'System.IO.IO', applying 'sequence' to a list of
-- IO actions, performs each in turn, returning a list of the results:
--
-- > sequence [putStr "Hello ", putStrLn "World!"]
-- >     = (\a b -> [a,b]) <$> putStr "Hello " <*> putStrLn "World!"
-- >     = do u1 <- putStr "Hello "
-- >          u2 <- putStrLn "World!"
-- >          return [u1, u2]         -- In this case  [(), ()]
--
-- For 'sequenceA', the /non-deterministic/ behaviour of @List@ is most easily
-- seen in the case of a list of lists (of elements of some common fixed type).
-- The result is a cross-product of all the sublists:
--
-- >>> sequenceA [[0, 1, 2], [30, 40], [500]]
-- [[0,30,500],[0,40,500],[1,30,500],[1,40,500],[2,30,500],[2,40,500]]
--
-- Because the input list has three (sublist) elements, the result is a list of
-- triples (/same shape/).
--
-- When the monad __@m@__ is 'Maybe' or 'Either', the effect in question is to
-- short-circuit the computation on encountering 'Nothing' or 'Left'.
--
-- >>> sequence [Just 1,Just 2,Just 3]
-- Just [1,2,3]
-- >>> sequence [Just 1,Nothing,Just 3]
-- Nothing
-- >>> sequence [Right 1,Right 2,Right 3]
-- Right [1,2,3]
-- >>> sequence [Right 1,Left "sorry",Right 3]
-- Left "sorry"
--
-- The result of 'sequence' is all-or-nothing, either structures of exactly the
-- same shape as the input or none at all.  The 'sequence' function does not
-- perform selective filtering as with e.g. 'Data.Maybe.catMaybes' or
-- 'Data.Either.rights':
--
-- >>> catMaybes [Just 1,Nothing,Just 3]
-- [1,3]
-- >>> rights [Right 1,Left "sorry",Right 3]
-- [1,3]
--
-- The 'traverse' and 'mapM' functions can in principle be implemented in terms
-- of 'sequenceA' and 'sequence' respectively:
--
-- > traverse g = sequenceA . fmap g
-- > mapM g ts = sequence $ fmap g ts
--
-- but this is not recommended, it requires that the structure is already
-- independently a 'Functor', and 'sequenceA' expressed as __@traverse id@__ is
-- much simpler than 'traverse' expressed via a composition of 'sequenceA' and
-- 'fmap'.  Instances should generally implement 'traverse' explicitly.  It may
-- in some cases also make sense to implement a specialised 'mapM'.
--
-- Because 'fmapDefault' is defined in terms of 'traverse' (whose default
-- definition in terms of 'sequenceA' uses 'fmap'), you must not use
-- 'fmapDefault' to define the @Functor@ instance if the @Traversable@ instance
-- directly defines only 'sequenceA'.

------------------

-- $tree_instance
--
-- #tree#
-- The definition of a 'Traversable' instance for a binary tree is rather
-- similar to the corresponding instance of 'Functor', given the data type:
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a canonical @Functor@ instance would be
--
-- > instance Functor Tree where
-- >    fmap g Empty        = Empty
-- >    fmap g (Leaf x)     = Leaf (g x)
-- >    fmap g (Node l k r) = Node (fmap g l) (g k) (fmap g r)
--
-- a canonical @Traversable@ instance would be
--
-- > instance Traversable Tree where
-- >    traverse g Empty        = pure Empty
-- >    traverse g (Leaf x)     = Leaf <$> g x
-- >    traverse g (Node l k r) = Node <$> traverse g l <*> g k <*> traverse g r
--
-- This definition works for any __@g :: a -> f b@__, with __@f@__ an
-- Applicative functor, as the laws for @('<*>')@ imply the requisite
-- associativity.
--
-- We can add an explicit non-default 'mapM' if desired:
--
-- >    mapM g Empty        = return Empty
-- >    mapM g (Leaf x)     = Leaf <$> g x
-- >    mapM g (Node l k r) = do
-- >        ml <- mapM g l
-- >        mk <- g k
-- >        mr <- mapM g r
-- >        return $ Node ml mk mr
--
-- See [Construction](#construction) below for a more detailed exploration of
-- the general case, but as mentioned in [Overview](#overview) above, instance
-- definitions are typically rather simple, all the interesting behaviour is a
-- result of an interesting choice of 'Applicative' functor for a traversal.

------------------

-- $construction
--
-- How do @Traversable@ functors manage to construct a new container of the
-- same shape by sequencing effects over their elements?  Well, left-to-right
-- traversal with sequencing of effects suggests induction from a base case, so
-- the first question is what is the base case?  A @Traversable@ container with
-- elements of type __@a@__ generally has some minimal form that is either
-- "empty" or has just a single element (think "Data.List" vs.
-- "Data.List.Nonempty").
--
-- * If the base case is empty (no associated first value of __@a@__) then
--   traversal just reproduces the empty structure with no side effects,
--   so we have:
--
--     > traverse _ empty = pure empty
--
--     With the List monad, "empty" is __@[]@__, while with 'Maybe' it is
--     'Nothing'.  With __@Either e a@__ we have an /empty/ case for each
--     value of __@e@__.
--
-- * If the base case is a __@singleton a@__, then 'traverse' can take that
--   __@a@__, apply __@f :: a -> F b@__ getting an __@F b@__, then
--   __@fmap singleton@__ over that, getting __@F (singleton b)@__:
--
--     > traverse f (singleton a) = singleton <$> f a
--
-- Since 'Maybe' and 'Either' are either empty or singletons, we have
--
-- > traverse _ Nothing = pure Nothing
-- > traverse f (Just a) = Just <$> f a
--
-- > traverse _ (Left e) = pure (Left e)
-- > traverse f (Right a) = Right <$> f a
--
-- Similarly, for List, we have:
--
-- > traverse f [] = pure []
-- > traverse f [a] = fmap (:[]) (f a) = (:) <$> f a <*> pure []
--
-- What remains to be done is an inductive step beyond the empty and singleton
-- cases.  For a concrete @Traversable@ functor @T@ we need to be able to
-- extend our structure incrementally by filling in holes.  We can view a
-- partially built structure __@t0 :: T a@__ as a function
-- __@append :: a -> T a@__ that takes one more element __@a@__ to insert into
-- the container to the right of the existing elements to produce a larger
-- structure.  Conversely, we can view an element @a@ as a function
-- __@prepend :: T a -> T a@__ of a partially built structure that inserts the
-- element to the left of the existing elements.
--
-- Assuming that 'traverse' has already been defined on the partially built
-- structure:
--
-- > f0 = traverse f t0 :: F (T b)
--
-- we aim to define __@traverse f (append t0 a)@__ and/or
-- __@traverse f (prepend a t0)@__.
--
-- We can lift @append@ and apply it to @f0@ to get:
--
-- > append <$> f0 :: F (b -> T b)
--
-- and from the /next/ element __@a@__ we can obtain __@f a :: F b@__, and
-- this is where we'll make use of the applicative instance of @F@.  Adding
-- one more element on the right is then:
--
-- > traverse f (append t0 a) = append <$> traverse f t0 <*> f a
--
-- while prepending an element on the left is:
--
-- > traverse f (prepend a t0) = prepend <$> f a <*> traverse f t0
--
-- The (binary) @Tree@ instance example makes use of both, after defining the
-- @Empty@ base case and the singleton @Leaf@ node case, non-empty internal
-- nodes introduce both a prepended child node on the left and an appended
-- child node on the right:
--
-- > traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- The above definitions sequence the 'Applicative' effects of __@F@__ in the
-- expected order while producing results of the expected shape __@T@__.
--
-- For lists we get the natural order of effects by using
-- __@(prepend \<$\> f a)@__ as the operator and __@(traverse f as)@__ as the
-- operand (the actual definition is written as an equivalent right fold
-- in order to enable /fusion/ rules):
--
-- > traverse f [] = pure []
-- > traverse f (a:as) = (:) <$> f a <*> traverse f as
--
-- The origin of the combinatorial product when __@F@__ is __@[]@__ should now
-- be apparent, the /non-deterministic/ definition of @\<*\>@ for @List@ makes
-- multiple independent choices for each element of the structure.

------------------

-- $laws
--
-- #laws#
-- A definition of 'traverse' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'traverse' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'traverse' ('Data.Functor.Compose.Compose' . 'fmap' g . f)
--     = 'Data.Functor.Compose.Compose' . 'fmap' ('traverse' g) . 'traverse' f@
--
-- A definition of 'sequenceA' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'sequenceA' . 'fmap' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'sequenceA' . 'fmap' 'Data.Functor.Compose.Compose'
--     = 'Data.Functor.Compose.Compose' . 'fmap' 'sequenceA' . 'sequenceA'@
--
-- where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- @
-- t ('pure' x) = 'pure' x
-- t (f '<*>' x) = t f '<*>' t x
-- @
--
-- and the identity functor 'Identity' and composition functors
-- 'Data.Functor.Compose.Compose' are from "Data.Functor.Identity" and
-- "Data.Functor.Compose".
--
-- A result of the naturality law is a purity law for 'traverse'
--
-- @'traverse' 'pure' = 'pure'@
--
-- (The naturality law is implied by parametricity and thus so is the
-- purity law [1, p15].)
--
-- The superclass instances should satisfy the following:
--
--  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
--    with the identity applicative functor ('fmapDefault').
--
--  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
--    equivalent to traversal with a constant applicative functor
--    ('foldMapDefault').
--
-- Note: the 'Functor' superclass means that (in GHC) Traversable structures
-- cannot impose any constraints on the element type.  A Haskell implementation
-- that supports constrained functors could make it possible to define
-- constrained @Traversable@ structures.

------------------

-- $also
--
--  * [1] \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/#iterator>.
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
