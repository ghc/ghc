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
-- performing an action on each element.
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

    -- *** Their 'Foldable', just the effects, analogues.
    -- $effectful

    -- ** The 'sequenceA' and 'sequence' methods
    -- $sequence

    -- ** Example binary tree instance
    -- $tree_instance

    -- ** Construction
    --
    -- $construction

    -- * Specialised traversals

    -- ** Identity: the 'fmapDefault' function
    -- $identity

    -- ** State: the 'mapAccumL', 'mapAccumR' functions
    -- $stateful

    -- ** Const: the 'foldMapDefault' function
    -- $phantom

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
-- support element-wise left-to-right sequencing of 'Applicative' (or 'Monad')
-- effects whose return values fill in new structures of __the same shape__
-- as the input.
--
-- The sequencing of effects in @Traversable@ structures is rather
-- straight-forward, see the [Construction](#construction) section for details.
-- The diverse uses of @Traversable@ structures result from the many possible
-- choices of Applicative effects to thread through a traversable structure.
--
-- After exploring the basic 'Traversable' methods in more detail, and
-- explaining how instances are constructed, we'll take a look at some examples
-- of using various Applicative effects to implement the auxiliary functions in
-- this module.
--
-- A @Traversable@ structure is automatically both a 'Functor' and 'Foldable'.
-- These superclasses do not impose additional limitations on the the set of
-- structures that can support the @Traversable@ interface because natural
-- implementations of both 'fmap' and 'foldMap' (also 'foldr', ...) are
-- available in terms of 'traverse'.
--
-- The 'Functor' superclass means that the container cannot impose any
-- constraints on the element type, so containers that require elements to be
-- comparable, or hashable, etc., cannot be instances of the @Traversable@
-- class.
--
-- When for a new structure there's no compelling reason to directly implement
-- the @Functor@ and/or @Foldable@ methods, one can use either or both of
-- 'fmapDefault' and 'foldMapDefault' to define the requisite instances.  It is
-- then sufficient to implement just 'traverse'.  Direct implementation of
-- fine-tuned superclass methods may of course produce more efficient code.
--
-- Because 'fmapDefault' is defined in terms of 'traverse' (whose default
-- definition in terms of 'sequenceA' uses 'fmap'), you must not use
-- 'fmapDefault' to define the @Functor@ instance if the @Traversable@ instance
-- directly defines only 'sequenceA'.

------------------

-- $traverse
-- For an 'Applicative' functor __@f@__ and a @Traversable@ structure __@t@__,
-- the type signatures of 'traverse' and 'fmap' are rather similar:
--
-- > fmap     :: (a -> f b) -> t a -> t (f b)
-- > traverse :: (a -> f b) -> t a -> f (t b)
--
-- with one crucial difference: 'fmap' produces a structure whose elements (of
-- type __@f b@__) are as-yet unevaluated effects, while 'traverse' produces an
-- aggregate effect yielding zero or more structures of type __@t b@__.  If any
-- such structures are produced, they all have /the same shape/ (list length,
-- graph of tree nodes, ...) as the input structure __@t a@__, but the slots
-- previously occupied by elements of type __@a@__ now hold elements of type
-- __@b@__.
--
-- For example, when __@f@__ is the __@IO@__ monad, and __@t@__ is @List@,
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
-- 'traverse' function does not perform selective pruning of slots in the output
-- structure as with e.g. 'Data.Maybe.mapMaybe'.
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
-- same can happen when __@f@__ is @List@ and the result is __@[]@__, or
-- __@f@__ is __@Either e@__ and the result is __@Left (x :: e)@__, or perhaps
-- the 'Control.Applicative.empty' value of some
-- 'Control.Applicative.Alternative' functor.
--
-- More than one __@t b@__ structure can be produced when __@f b@__ can hold
-- more than one value of type __@b@__.  When __@f@__ is e.g. @List@, and the
-- map __@g :: a -> [b]@__ returns more than one result for some inputs __@a@__
-- (and at least one for all __@a@__), the result of __@traverse g ts@__ will
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
-- In other words, traversal with a function __@g :: a -> [b]@__, over an input
-- structure __@t a@__, yields a list __@[t b]@__, whose length is the product
-- of the lengths of the lists that @g@ returns for each element of the input
-- structure!  The individual elements __@a@__ of the structure are replaced by
-- each element of __@g a@__ in turn:
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

-- $effectful
--
-- The 'traverse' and 'mapM' methods have analogues in the "Data.Foldable"
-- modules.  These are 'traverse_' and 'mapM_', and their flipped variants
-- 'for_' and 'forM_', respectively.  The result type is __@f ()@__, they don't
-- return an updated structure, and can be used to sequence effects over all
-- the elements of a @Traversable@ (any 'Foldable') structure, just for their
-- side-effects.
--
-- When effects can short-circuit, the result of these functions is not always
-- __@pure ()@__, it may instead be 'Nothing' when __@f@__ is __'Maybe'__, or
-- perhaps __@Left e@__ when it is __@'Either' e@__, and so on.  If the
-- @Traversable@ structure is empty, the result is always __@pure ()@__.
--
-- It is perhaps worth noting that 'Maybe' is not only a potential
-- 'Applicative' functor for the return value of the first argument of
-- 'traverse', but is also itself a 'Traversable' structure with either zero or
-- one element.  A convenient idiom for conditionally executing an action just
-- for its effects on a 'Just' value, and doing nothing otherwise is:
--
-- > -- action :: Monad m => a -> m ()
-- > -- mvalue :: Maybe a
-- > mapM_ action mvalue -- :: m ()
--
-- which is more concise than:
--
-- > maybe (return ()) action mvalue
--
-- The 'mapM_' idiom works verbatim if the type of __@mvalue@__ is later
-- refactored from __@Maybe a@__ to __@Either e a@__ (assuming it remains OK to
-- silently do nothing in the 'Left' case).

------------------

-- $sequence
-- The 'sequenceA' and 'sequence' methods are useful when what you have is a
-- container of pending applicative or monadic effects, and you want to combine
-- then left-to-right into a single effect that produces zero or more containers
-- with the computed values.
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
-- 'fmap'.  Instances should generally explicitly implement 'traverse', and
-- perhaps also a specialised 'mapM'.

------------------

-- $tree_instance
--
-- The definition of a @Traversable' instance for a binary tree is rather
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
-- we can add an explicit non-default 'mapM' if desired:
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
-- #construction#
-- How do @Traversable@ functors manage to construct a new structure of the
-- same shape by sequencing effects over their elements?  Well, left-to-right
-- traversal with sequencing of effects suggests induction from a base case, so
-- the first question is what is the base case?  A @Traversable@ structure with
-- elements of type __@a@__ typically has some minimal form that is either
-- /empty/ or has just a single element (consider "Data.List" vs.
-- "Data.List.Nonempty").
--
-- * If the base case is empty (no first value of __@a@__) then
--   traversal just reproduces the empty structure with no side effects,
--   so we have:
--
--     > traverse _ empty = pure empty
--
--     With the List monad, /empty/ is __@[]@__, while with 'Maybe' it is
--     'Nothing'.  With __@Either e a@__ we have an /empty/ case for each
--     value of __@e@__:
--
--     > traverse _ (Left e) = pure $ Left e
--
--     Note, the two @Left@ values above generally have distinct types:
--     __@Either e a@__ vs. __@Either e b@__, so we must rebuild the value from
--     __@e@__, rather than reuse the __@Left e@__ term from the left side of
--     the equation.
--
-- * Given a hypothetical function __@singleton :: a -> t a@__ that constructs
--   a one-element traversable structure, if the base case is __@singleton a@__
--   for some element __@a@__, then 'traverse' can take that __@a@__, apply
--   __@g :: a -> f b@__ getting an __@f b@__, then __@fmap singleton@__ over
--   that, getting an __@f (t b)@__ as required:
--
--     > traverse g (singleton a) = singleton <$> g a
--
-- Since 'Maybe' and 'Either' are either empty or singletons, we have
--
-- > traverse _ Nothing = pure Nothing
-- > traverse g (Just a) = Just <$> g a
--
-- > traverse _ (Left e) = pure (Left e)
-- > traverse g (Right a) = Right <$> g a
--
-- Similarly, for List, we have:
--
-- > traverse _ []  = pure []
-- > traverse g [a] = fmap (:[]) (g a)
-- >                = (:) <$> (g a) <*> traverse g []
-- >                = liftA2 (:) (g a) (traverse g [])
--
-- What remains to be done is an inductive step beyond the empty and singleton
-- cases.  For a given @Traversable@ structure __@t@__ we need to be able to
-- extend our structure incrementally by filling in holes.
--
-- Let __@append :: t x -> x -> t x@__ be a function that takes a partially
-- built structure __@t0 :: t x@__ and one more element __@x@__ to add to the
-- right of the existing elements to produce a larger structure.  Conversely,
-- let __@prepend :: x -> t x -> t x@__ be a function that adds one more
-- element to the left of existing elements of a partially built structure.
--
-- Assuming that 'traverse' has already been defined on the partially built
-- structure __@t0@__:
--
-- > g0 = traverse g t0 :: f (t b)
--
-- we aim to define __@traverse g (append t0 a)@__ and/or
-- __@traverse g (prepend a t0)@__.
--
-- We can lift __@append@__ and apply it to __@g0@__ to get:
--
-- > append <$> g0 :: f (b -> t b)
--
-- and from the /next/ element __@a@__ we can obtain __@g a :: f b@__, and
-- make use of the Applicative instance of __@f@__ to combine these.  Adding
-- one more element on the right therefore becomes:
--
-- > traverse g (append t0 a) = append <$> traverse g t0 <*> g a
-- >                          = liftA2 append g0 (g a)
--
-- while adding an element on the left is similarly:
--
-- > traverse g (prepend a t0) = prepend <$> (g a) <*> traverse g t0
-- >                           = liftA2 prepend (g a) g0
--
-- The origin of the combinatorial product when __@f@__ is @List@ should now
-- be apparent, the /non-deterministic/ definition of @('<*>')@ for @List@ makes
-- multiple independent choices for each element of the structure.
--
-- For the (binary) [Tree instance](#tree_instance), after handlingg the
-- __@Empty@__ base case and the singleton __@Leaf@__ node case, non-empty
-- internal nodes require both a prepended child node on the left and an
-- appended child node on the right:
--
-- > traverse g (Node l k r) = Node <$> traverse g l <*> g k <*> traverse g r
--
-- The above definitions sequence the 'Applicative' effects of __@f@__ in the
-- expected order while producing results of the expected shape __@t@__.
--
-- For another concrete example, when __@t@__ is @List@, we get the natural order
-- of effects by lifting @(':')@ to affect a prepend operation:
--
-- > traverse g [] = pure []
-- > traverse g (x:xs) = liftA2 (:) (g a) (traverse g xs)
--
-- The actual definition is written as an equivalent right fold in order to
-- facilitate list /fusion/:
--
-- > traverse g = List.foldr cons_g (pure [])
-- >   where cons_g x ys = liftA2 (:) (g x) ys

------------------

-- $identity
--
-- #identity#
-- The simplest Applicative functor is 'Identity', which just wraps and unwraps
-- pure values and function application.  This allows us to define
-- 'fmapDefault':
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > import Data.Coercible (coerce)
-- >
-- > type TraverseI t a b = (a -> Identity b) -> t a -> Identity (t b)
-- >
-- > fmapDefault :: forall t a b. Traversable t => (a -> b) -> t a -> t b
-- > fmapDefault = coerce (traverse :: TraverseI t a b)
--
-- The use of 'coerce' avoids allocations that merely wrap and unwrap
-- __@newtype@__ terms, which have the same runtime representation and can be
-- reused as-is.  This reduces memory allocations and GC workload.
--
-- As noted in [Overview](#overview), 'fmapDefault' can only be used to define
-- the requisite 'Functor' instance of a 'Traversable' structure when the
-- 'traverse' method is explicitly implemented.  An infinite loop would result
-- if in addition 'traverse' were defined in terms of 'sequenceA' and 'fmap'.

------------------

-- $stateful
--
-- #stateful#
-- Applicative functors that thread a changing state through a computation are
-- an interesting use-case for 'traverse'.  The 'mapAccumL' and 'mapAccumR'
-- functions in this module are each defined in terms of such traversals.
--
-- We first define a simplified (not a monad transformer) version of
-- 'Control.Monad.Trans.State.State' that threads a state __@s@__ through a
-- chain of computations left to right.  Its @('<*>')@ operator passes the
-- input state first to its left argument, and then the resulting state is
-- passed to its right argument, which in returns the final state.
--
-- > newtype StateL s a = StateL { runStateL :: s -> (s, a) }
-- >
-- > instance Functor (StateL s) where
-- >     fmap f (StateL kx) = StateL $ \ s ->
-- >         let (s', x) = kx s in (s', f x)
-- >
-- > instance Applicative (StateL s) where
-- >     pure a = StateL $ \s -> (s, a)
-- >     (StateL kf) <*> (StateL kx) = StateL $ \ s ->
-- >         let { (s',  f) = kf s
-- >             ; (s'', x) = kx s' } in (s'', f x)
-- >     liftA2 f (StateL kx) (StateL ky) = StateL $ \ s ->
-- >         let { (s',  x) = kx s
-- >             ; (s'', y) = ky s' } in (s'', f x y)
--
-- With @StateL@, we can define 'mapAccumL' as follows:
--
-- > type TraverseL t s a b = (a -> StateL s b) -> t a -> StateL s (t b)
-- >
-- > mapAccumL :: forall t s a b. Traversable t
-- >           => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- > mapAccumL g s ts = coerce (traverse :: TraverseL t s a b) (flip g) ts s
--
-- The type of __@StateL . flip g@__ is __@a -> StateL b@__, which makes it
-- suitable for use with 'traverse', and during the Applicative
-- [construction](#construction) of __@StateL (t b)@__ the state updates will
-- thread left-to-right along the sequence of elements of __@t a@__.
--
-- As in the section on [Identity](#identity), the use of 'coerce' avoids
-- allocations that merely wrap and unwrap __@newtype@__ terms.
--
-- While 'mapAccumR' has a type signature identical to 'mapAccumL', it differs
-- in the expected order of evaluation of effects, which must take place
-- right-to-left.
--
-- For this we need a variant control structure @StateR@, which threads the
-- state right-to-left, by passing the input state to its right argument and
-- then using the resulting state as an input to its left argument:
--
-- > newtype StateR s a = StateR { runStateR :: s -> (s, a) }
-- >
-- > instance Functor (StateR s) where
-- >     fmap f (StateR kx) = StateR $ \s ->
-- >         let (s', x) = kx s in (s', f x)
-- >
-- > instance Applicative (StateR s) where
-- >     pure a = StateR $ \s -> (s, a)
-- >     (StateR kf) <*> (StateR kx) = StateR $ \ s ->
-- >         let { (s',  x) = kx s
-- >             ; (s'', f) = kf s' } in (s'', f x)
-- >     liftA2 f (StateR kx) (StateR ky) = StateR $ \ s ->
-- >         let { (s',  y) = ky s
-- >             ; (s'', x) = kx s' } in (s'', f x y)
--
-- With @StateR@, we can define 'mapAccumR' as follows:
--
-- > type TraverseR t s a b = (a -> StateR s b) -> t a -> StateR s (t b)
-- >
-- > mapAccumR :: forall t s a b. Traversable t
-- >           => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- > mapAccumR g s0 ts = coerce (traverse :: TraverseR t s a b) (flip g) ts s0
--
-- Various stateful traversals can be constructed from 'mapAccumL' and
-- 'mapAccumR' for suitable choices of @g@, or built directly along similar
-- lines.

------------------

-- $phantom
--
-- #phantom#
-- The 'Const' Functor enables applications of 'traverse' that summarise the
-- input structure to an output value without constructing any output values
-- of the same type or shape.
--
-- As noted [above](#overview), the @Foldable@ superclass constraint is
-- justified by the fact that it is possible to construct 'foldMap', 'foldr',
-- etc., from 'traverse'.  The technique used is useful in its own right, and
-- is explored below.
--
-- A key feature of folds is that they can reduce the input structure to a
-- summary value. Often neither the input structure nor a mutated clone is
-- needed once the fold is computed, and through list fusion the input may not
-- even have been memory resident in its entirety at the same time.
--
-- The 'traverse' method does not at first seem to be a suitable building block
-- for folds, because its return value __@f (t b)@__ appears to retain mutated
-- copies of the input structure.  But the presence of __@t b@__ in the type
-- signature need not mean that terms of type __@t b@__ are actually embedded
-- in __@f (t b)@__.  The simplest way to elide the excess terms is by basing
-- the Applicative functor used with 'traverse' on 'Const'.
--
-- Not only does __@Const a b@__ hold just an __@a@__ value, with the __@b@__
-- parameter merely a /phantom/ type, but when __@m@__ has a 'Monoid' instance,
-- __@Const m@__ is an 'Applicative' functor:
--
-- > import Data.Coerce (coerce)
-- > newtype Const a b = Const { getConst :: a } deriving (Eq, Ord, Show) -- etc.
-- > instance Functor (Const m) where fmap = const coerce
-- > instance Monoid m => Applicative (Const m) where
-- >    pure _   = Const mempty
-- >    (<*>)    = coerce (mappend :: m -> m -> m)
-- >    liftA2 _ = coerce (mappend :: m -> m -> m)
--
-- We can therefore define a specialisation of 'traverse':
--
-- > type TraverseC t a m = (a -> Const m ()) -> t a -> Const m (t ())
-- > traverseC :: forall t a m. (Monoid m, Traversable t) => TraverseC t a m
-- > traverseC = traverse
--
-- For which the Applicative [construction](#construction) of 'traverse'
-- leads to:
--
-- prop> null ts ==> traverseC g ts = Const mempty
-- prop> traverseC g (prepend x xs) = Const (g x) <> traverseC g xs
--
-- In other words, this makes it possible to define:
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > foldMapDefault :: forall t a m. (Monoid m, Traversable t) => (a -> m) -> t a -> m
-- > foldMapDefault = coerce (traverse :: TraverseC t a m)
--
-- Which is sufficient to define a 'Foldable' superclass instance:
--
-- > instance Traversable t => Foldable t where foldMap = foldMapDefault
--
-- As in the section on [Identity](#identity), the use of 'coerce' avoids
-- allocations that merely wrap and unwrap __@newtype@__ terms.
--
-- It may however be instructive to also directly define candidate default
-- implementations of 'foldr' and 'foldl'', which take a bit more machinery
-- to construct:
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > import Data.Coerce (coerce)
-- > import Data.Functor.Const (Const(..))
-- > import Data.Semigroup (Dual(..), Endo(..))
-- > import GHC.Exts (oneShot)
-- >
-- > type TraverseR t a b =
-- >     (a -> Const (Endo b) ()) -> t a -> Const (Endo b) (t ())
-- > type TraverseL t a b =
-- >     (a -> Const (Dual (Endo b)) ()) -> t a -> Const (Dual (Endo b)) (t ())
-- >
-- > foldrDefault :: forall t a b. Traversable t => (a -> b -> b) -> b -> t a -> b
-- > foldrDefault f z = \t -> coerce (traverse :: TraverseR t a b) f t z
-- >
-- > foldlDefault' :: forall t a b. Traversable t => (b -> a -> b) -> b -> t a -> b
-- > foldlDefault' f z = \t -> coerce (traverse :: TraverseL t a b) f' t z
-- >   where
-- >     f' :: a -> b -> b
-- >     f' a = oneShot $ \ b -> b `seq` f b a
--
-- In the above we're using the __@Endo b@__ 'Monoid' to compose a sequence of
-- __@b -> b@__ accumulator updates in either left-to-right or right-to-left
-- order.
--
-- As in the section on [fmapDefault](#identity), the use of 'coerce' avoids
-- allocations that merely wrap and unwrap __@newtype@__ terms.
--
-- The use of 'seq' in the definition of __@foldlDefault'@__ ensures strictness
-- in the accumulator.
--
-- The 'GHC.Exts.oneShot' function gives a hint to the compiler that aids in
-- correct optimisation of lambda terms that fire at most once (for each
-- element __@a@__) and so should not try to pre-compute and re-use
-- subexpressions that pay off only on repeated execution.  Otherwise, it is
-- just the identity function.

------------------

-- $laws
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
