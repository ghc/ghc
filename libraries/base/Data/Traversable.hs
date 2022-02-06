{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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

    -- *** Their 'Foldable', just the effects, analogues.
    -- $effectful

    -- *** Result multiplicity
    -- $multiplicity

    -- ** The 'sequenceA' and 'sequence' methods
    -- $sequence

    -- *** Care with default method implementations
    -- $seqdefault

    -- *** Monadic short circuits
    -- $seqshort

    -- ** Example binary tree instance
    -- $tree_instance

    -- *** Pre-order and post-order tree traversal
    -- $tree_order

    -- ** Making construction intuitive
    --
    -- $construction

    -- * Advanced traversals
    -- $advanced

    -- *** Coercion
    -- $coercion

    -- ** Identity: the 'fmapDefault' function
    -- $identity

    -- ** State: the 'mapAccumL', 'mapAccumR' functions
    -- $stateful

    -- ** Const: the 'foldMapDefault' function
    -- $phantom

    -- ** ZipList: transposing lists of lists
    -- $ziplist

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

-- | Functors representing data structures that can be transformed to
-- structures of the /same shape/ by performing an 'Applicative' (or,
-- therefore, 'Monad') action on each element from left to right.
--
-- A more detailed description of what /same shape/ means, the various methods,
-- how traversals are constructed, and example advanced use-cases can be found
-- in the __Overview__ section of "Data.Traversable#overview".
--
-- For the class laws see the __Laws__ section of "Data.Traversable#laws".
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
mapAccumL :: forall t s a b. Traversable t
          => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- See Note [Function coercion] in Data.Functor.Utils.
mapAccumL f s t = coerce (traverse @t @(StateL s) @a @b) (flip f) t s

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
mapAccumR :: forall t s a b. Traversable t
          => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- See Note [Function coercion] in Data.Functor.Utils.
mapAccumR f s t = coerce (traverse @t @(StateR s) @a @b) (flip f) t s

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
fmapDefault = coerce (traverse @t @Identity @a @b)

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
foldMapDefault = coerce (traverse @t @(Const m) @a @())

------------------

-- $overview
--
-- #overview#
-- Traversable structures support element-wise sequencing of 'Applicative'
-- effects (thus also 'Monad' effects) to construct new structures of
-- __the same shape__ as the input.
--
-- To illustrate what is meant by /same shape/, if the input structure is
-- __@[a]@__, each output structure is a list __@[b]@__ of the same length as
-- the input.  If the input is a __@Tree a@__, each output __@Tree b@__ has the
-- same graph of intermediate nodes and leaves.  Similarly, if the input is a
-- 2-tuple __@(x, a)@__, each output is a 2-tuple __@(x, b)@__, and so forth.
--
-- It is in fact possible to decompose a traversable structure __@t a@__ into
-- its shape (a.k.a. /spine/) of type __@t ()@__ and its element list
-- __@[a]@__.  The original structure can be faithfully reconstructed from its
-- spine and element list.
--
-- The implementation of a @Traversable@ instance for a given structure follows
-- naturally from its type; see the [Construction](#construction) section for
-- details.
-- Instances must satisfy the laws listed in the [Laws section](#laws).
-- The diverse uses of @Traversable@ structures result from the many possible
-- choices of Applicative effects.
-- See the [Advanced Traversals](#advanced) section for some examples.
--
-- Every @Traversable@ structure is both a 'Functor' and 'Foldable' because it
-- is possible to implement the requisite instances in terms of 'traverse' by
-- using 'fmapDefault' for 'fmap' and 'foldMapDefault' for 'foldMap'.  Direct
-- fine-tuned implementations of these superclass methods can in some cases be
-- more efficient.

------------------

-- $traverse
-- For an 'Applicative' functor __@f@__ and a @Traversable@ functor __@t@__,
-- the type signatures of 'traverse' and 'fmap' are rather similar:
--
-- > fmap     :: (a -> f b) -> t a -> t (f b)
-- > traverse :: (a -> f b) -> t a -> f (t b)
--
-- The key difference is that 'fmap' produces a structure whose elements (of
-- type __@f b@__) are individual effects, while 'traverse' produces an
-- aggregate effect yielding structures of type __@t b@__.
--
-- For example, when __@f@__ is the __@IO@__ monad, and __@t@__ is __@List@__,
-- 'fmap' yields a list of IO actions, whereas 'traverse' constructs an IO
-- action that evaluates to a list of the return values of the individual
-- actions performed left-to-right.
--
-- > traverse :: (a -> IO b) -> [a] -> IO [b]
--
-- The 'mapM' function is a specialisation of 'traverse' to the case when
-- __@f@__ is a 'Monad'.  For monads, 'mapM' is more idiomatic than 'traverse'.
-- The two are otherwise generally identical (though 'mapM' may be specifically
-- optimised for monads, and could be more efficient than using the more
-- general 'traverse').
--
-- > traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- > mapM     :: (Monad       m, Traversable t) => (a -> m b) -> t a -> m (t b)
--
-- When the traversable term is a simple variable or expression, and the
-- monadic action to run is a non-trivial do block, it can be more natural to
-- write the action last.  This idiom is supported by 'for' and 'forM', which
-- are the flipped versions of 'traverse' and 'mapM', respectively.

------------------

-- $multiplicity
--
-- #multiplicity#
-- When 'traverse' or 'mapM' is applied to an empty structure __@ts@__ (one for
-- which __@'null' ts@__ is 'True') the return value is __@pure ts@__
-- regardless of the provided function __@g :: a -> f b@__.  It is not possible
-- to apply the function when no values of type __@a@__ are available, but its
-- type determines the relevant instance of 'pure'.
--
-- prop> null ts ==> traverse g ts == pure ts
--
-- Otherwise, when __@ts@__ is non-empty and at least one value of type __@b@__
-- results from each __@f a@__, the structures __@t b@__ have /the same shape/
-- (list length, graph of tree nodes, ...) as the input structure __@t a@__,
-- but the slots previously occupied by elements of type __@a@__ now hold
-- elements of type __@b@__.
--
-- A single traversal may produce one, zero or many such structures.  The zero
-- case happens when one of the effects __@f a@__ sequenced as part of the
-- traversal yields no replacement values.  Otherwise, the many case happens
-- when one of sequenced effects yields multiple values.
--
-- The 'traverse' function does not perform selective filtering of slots in the
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
-- When __@f@__ is e.g. __@List@__, and the map __@g :: a -> [b]@__ returns
-- more than one value for some inputs __@a@__ (and at least one for all
-- __@a@__), the result of __@mapM g ts@__ will contain multiple structures of
-- the same shape as __@ts@__:
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

-- $effectful
-- #effectful#
--
-- The 'traverse' and 'mapM' methods have analogues in the "Data.Foldable"
-- module.  These are 'traverse_' and 'mapM_', and their flipped variants
-- 'for_' and 'forM_', respectively.  The result type is __@f ()@__, they don't
-- return an updated structure, and can be used to sequence effects over all
-- the elements of a @Traversable@ (any 'Foldable') structure just for their
-- side-effects.
--
-- If the @Traversable@ structure is empty, the result is __@pure ()@__.  When
-- effects short-circuit, the __@f ()@__ result may, for example, be 'Nothing'
-- if __@f@__ is 'Maybe', or __@'Left' e@__ when it is __@'Either' e@__.
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
--
-- #sequence#
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

------------------

-- $seqshort
--
-- #seqshort#
-- When the monad __@m@__ is 'Either' or 'Maybe' (more generally any
-- 'Control.Monad.MonadPlus'), the effect in question is to short-circuit the
-- result on encountering 'Left' or 'Nothing' (more generally
-- 'Control.Monad.mzero').
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

------------------

-- $seqdefault
--
-- #seqdefault#
-- The 'traverse' method has a default implementation in terms of 'sequenceA':
--
-- > traverse g = sequenceA . fmap g
--
-- but relying on this default implementation is not recommended, it requires
-- that the structure is already independently a 'Functor'.  The definition of
-- 'sequenceA' in terms of __@traverse id@__ is much simpler than 'traverse'
-- expressed via a composition of 'sequenceA' and 'fmap'.  Instances should
-- generally implement 'traverse' explicitly.  It may in some cases also make
-- sense to implement a specialised 'mapM'.
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

-- $tree_order
--
-- It is perhaps worth noting that the traversal defined above gives an
-- /in-order/ sequencing of the elements.  If instead you want either
-- /pre-order/ (parent first, then child nodes) or post-order (child nodes
-- first, then parent) sequencing, you can define the instance accordingly:
--
-- > inOrderNode :: Tree a -> a -> Tree a -> Tree a
-- > inOrderNode l x r = Node l x r
-- >
-- > preOrderNode :: a -> Tree a -> Tree a -> Tree a
-- > preOrderNode x l r = Node l x r
-- >
-- > postOrderNode :: Tree a -> Tree a -> a -> Tree a
-- > postOrderNode l r x = Node l x r
-- >
-- > -- Traversable instance with in-order traversal
-- > instance Traversable Tree where
-- >     traverse g t = case t of
-- >         Empty      -> pure Empty
-- >         Leaf x     -> Leaf <$> g x
-- >         Node l x r -> inOrderNode <$> traverse g l <*> g x <*> traverse g r
-- >
-- > -- Traversable instance with pre-order traversal
-- > instance Traversable Tree where
-- >     traverse g t = case t of
-- >         Empty      -> pure Empty
-- >         Leaf x     -> Leaf <$> g x
-- >         Node l x r -> preOrderNode <$> g x <*> traverse g l <*> traverse g r
-- >
-- > -- Traversable instance with post-order traversal
-- > instance Traversable Tree where
-- >     traverse g t = case t of
-- >         Empty      -> pure Empty
-- >         Leaf x     -> Leaf <$> g x
-- >         Node l x r -> postOrderNode <$> traverse g l <*> traverse g r <*> g x
--
-- Since the same underlying Tree structure is used in all three cases, it is
-- possible to use @newtype@ wrappers to make all three available at the same
-- time!  The user need only wrap the root of the tree in the appropriate
-- @newtype@ for the desired traversal order.  Tne associated instance
-- definitions are shown below (see [coercion](#coercion) if unfamiliar with
-- the use of 'coerce' in the sample code):
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- >
-- > -- Default in-order traversal
-- >
-- > import Data.Coerce (coerce)
-- > import Data.Traversable
-- >
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
-- > instance Functor  Tree where fmap    = fmapDefault
-- > instance Foldable Tree where foldMap = foldMapDefault
-- >
-- > instance Traversable Tree where
-- >     traverse _ Empty = pure Empty
-- >     traverse g (Leaf a) = Leaf <$> g a
-- >     traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r
-- >
-- > -- Optional pre-order traversal
-- >
-- > newtype PreOrderTree a = PreOrderTree (Tree a)
-- > instance Functor  PreOrderTree where fmap    = fmapDefault
-- > instance Foldable PreOrderTree where foldMap = foldMapDefault
-- >
-- > instance Traversable PreOrderTree where
-- >     traverse _ (PreOrderTree Empty)        = pure $ preOrderEmpty
-- >     traverse g (PreOrderTree (Leaf x))     = preOrderLeaf <$> g x
-- >     traverse g (PreOrderTree (Node l x r)) = preOrderNode
-- >         <$> g x
-- >         <*> traverse g (coerce l)
-- >         <*> traverse g (coerce r)
-- >
-- > preOrderEmpty :: forall a. PreOrderTree a
-- > preOrderEmpty = coerce (Empty @a)
-- > preOrderLeaf :: forall a. a -> PreOrderTree a
-- > preOrderLeaf = coerce (Leaf @a)
-- > preOrderNode :: a -> PreOrderTree a -> PreOrderTree a -> PreOrderTree a
-- > preOrderNode x l r = coerce (Node (coerce l) x (coerce r))
-- >
-- > -- Optional post-order traversal
-- >
-- > newtype PostOrderTree a = PostOrderTree (Tree a)
-- > instance Functor  PostOrderTree where fmap    = fmapDefault
-- > instance Foldable PostOrderTree where foldMap = foldMapDefault
-- >
-- > instance Traversable PostOrderTree where
-- >     traverse _ (PostOrderTree Empty)        = pure postOrderEmpty
-- >     traverse g (PostOrderTree (Leaf x))     = postOrderLeaf <$> g x
-- >     traverse g (PostOrderTree (Node l x r)) = postOrderNode
-- >         <$> traverse g (coerce l)
-- >         <*> traverse g (coerce r)
-- >         <*> g x
-- >
-- > postOrderEmpty :: forall a. PostOrderTree a
-- > postOrderEmpty = coerce (Empty @a)
-- > postOrderLeaf :: forall a. a -> PostOrderTree a
-- > postOrderLeaf = coerce (Leaf @a)
-- > postOrderNode :: PostOrderTree a -> PostOrderTree a -> a -> PostOrderTree a
-- > postOrderNode l r x = coerce (Node (coerce l) x (coerce r))
--
-- With the above, given a sample tree:
--
-- > inOrder :: Tree Int
-- > inOrder = Node (Node (Leaf 10) 3 (Leaf 20)) 5 (Leaf 42)
--
-- we have:
--
-- > import Data.Foldable (toList)
-- > print $ toList inOrder
-- > [10,3,20,5,42]
-- >
-- > print $ toList (coerce inOrder :: PreOrderTree Int)
-- > [5,3,10,20,42]
-- >
-- > print $ toList (coerce inOrder :: PostOrderTree Int)
-- > [10,20,3,42,5]
--
-- You would typically define instances for additional common type classes,
-- such as 'Eq', 'Ord', 'Show', etc.

------------------

-- $construction
--
-- #construction#
-- In order to be able to reason about how a given type of 'Applicative'
-- effects will be sequenced through a general 'Traversable' structure by its
-- 'traversable' and related methods, it is helpful to look more closely
-- at how a general 'traverse' method is implemented.  We'll look at how
-- general traversals are constructed primarily with a view to being able
-- to predict their behaviour as a user, even if you're not defining your
-- own 'Traversable' instances.
--
-- Traversable structures __@t a@__ are assembled incrementally from their
-- constituent parts, perhaps by prepending or appending individual elements of
-- type __@a@__, or, more generally, by recursively combining smaller composite
-- traversable building blocks that contain multiple such elements.
--
-- As in the [tree example](#tree) above, the components being combined are
-- typically pieced together by a suitable /constructor/, i.e. a function
-- taking two or more arguments that returns a composite value.
--
-- The 'traverse' method enriches simple incremental construction with
-- threading of 'Applicative' effects of some function __@g :: a -> f b@__.
--
-- The basic building blocks we'll use to model the construction of 'traverse'
-- are a hypothetical set of elementary functions, some of which may have
-- direct analogues in specific @Traversable@ structures.  For example, the
-- __@(':')@__ constructor is an analogue for lists of @prepend@ or the more
-- general @combine@.
--
-- > empty :: t a               -- build an empty container
-- > singleton :: a -> t a      -- build a one-element container
-- > prepend :: a -> t a -> t a -- extend by prepending a new initial element
-- > append  :: t a -> a -> t a -- extend by appending a new final element
-- > combine :: a1 -> a2 -> ... -> an -> t a -- combine multiple inputs
--
-- * An empty structure has no elements of type __@a@__, so there's nothing
--   to which __@g@__ can be applied, but since we need an output of type
--   __@f (t b)@__, we just use the 'pure' instance of __@f@__ to wrap an
--   empty of type __@t b@__:
--
--     > traverse _ (empty :: t a) = pure (empty :: t b)
--
--     With the List monad, /empty/ is __@[]@__, while with 'Maybe' it is
--     'Nothing'.  With __@Either e a@__ we have an /empty/ case for each
--     value of __@e@__:
--
--     > traverse _ (Left e :: Either e a) = pure $ (Left e :: Either e b)
--
-- * A singleton structure has just one element of type __@a@__, and
--   'traverse' can take that __@a@__, apply __@g :: a -> f b@__ getting an
--   __@f b@__, then __@fmap singleton@__ over that, getting an __@f (t b)@__
--   as required:
--
--     > traverse g (singleton a) = fmap singleton $ g a
--
--     Note that if __@f@__ is __@List@__ and __@g@__ returns multiple values
--     the result will be a list of multiple __@t b@__ singletons!
--
--     Since 'Maybe' and 'Either' are either empty or singletons, we have
--
--     > traverse _ Nothing = pure Nothing
--     > traverse g (Just a) = Just <$> g a
--
--     > traverse _ (Left e) = pure (Left e)
--     > traverse g (Right a) = Right <$> g a
--
--     For @List@, empty is __@[]@__ and @singleton@ is __@(:[])@__, so we have:
--
--     > traverse _ []  = pure []
--     > traverse g [a] = fmap (:[]) (g a)
--     >                = (:) <$> (g a) <*> traverse g []
--     >                = liftA2 (:) (g a) (traverse g [])
--
-- * When the structure is built by adding one more element via __@prepend@__
--   or __@append@__, traversal amounts to:
--
--     > traverse g (prepend a t0) = prepend <$> (g a) <*> traverse g t0
--     >                           = liftA2 prepend (g a) (traverse g t0)
--
--     > traverse g (append t0 a) = append <$> traverse g t0 <*> g a
--     >                          = liftA2 append (traverse g t0) (g a)
--
--     The origin of the combinatorial product when __@f@__ is @List@ should now
--     be apparent, when __@traverse g t0@__ has __@n@__ elements and __@g a@__
--     has __@m@__ elements, the /non-deterministic/ 'Applicative' instance of
--     @List@ will produce a result with __@m * n@__ elements.
--
-- * When combining larger building blocks, we again use __@('<*>')@__ to
--   combine the traversals of the components.  With bare elements __@a@__
--   mapped to __@f b@__ via __@g@__, and composite traversable
--   sub-structures transformed via __@traverse g@__:
--
--     > traverse g (combine a1 a2 ... an) =
--     >     combine <$> t1 <*> t2 <*> ... <*> tn
--     >   where
--     >      t1 = g a1          -- if a1 fills a slot of type @a@
--     >         = traverse g a1 -- if a1 is a traversable substructure
--     >      ... ditto for the remaining constructor arguments ...
--
-- The above definitions sequence the 'Applicative' effects of __@f@__ in the
-- expected order while producing results of the expected shape __@t@__.
--
-- For lists this becomes:
--
-- > traverse g [] = pure []
-- > traverse g (x:xs) = liftA2 (:) (g a) (traverse g xs)
--
-- The actual definition of 'traverse' for lists is an equivalent
-- right fold in order to facilitate list /fusion/.
--
-- > traverse g = foldr (\x r -> liftA2 (:) (g x) r) (pure [])

------------------

-- $advanced
--
-- #advanced#
-- In the sections below we'll examine some advanced choices of 'Applicative'
-- effects that give rise to very different transformations of @Traversable@
-- structures.
--
-- These examples cover the implementations of 'fmapDefault', 'foldMapDefault',
-- 'mapAccumL' and 'mapAccumR' functions illustrating the use of 'Identity',
-- 'Const' and stateful 'Applicative' effects.  The [ZipList](#ziplist) example
-- illustrates the use of a less-well known 'Applicative' instance for lists.
--
-- This is optional material, which is not essential to a basic understanding of
-- @Traversable@ structures.  If this is your first encounter with @Traversable@
-- structures, you can come back to these at a later date.

-- $coercion
--
-- #coercion#
-- Some of the examples make use of an advanced Haskell feature, namely
-- @newtype@ /coercion/.  This is done for two reasons:
--
-- * Use of 'coerce' makes it possible to avoid cluttering the code with
--   functions that wrap and unwrap /newtype/ terms, which at runtime are
--   indistinguishable from the underlying value.  Coercion is particularly
--   convenient when one would have to otherwise apply multiple newtype
--   constructors to function arguments, and then peel off multiple layers
--   of same from the function output.
--
-- * Use of 'coerce' can produce more efficient code, by reusing the original
--   value, rather than allocating space for a wrapped clone.
--
-- If you're not familiar with 'coerce', don't worry, it is just a shorthand
-- that, e.g., given:
--
-- > newtype Foo a = MkFoo { getFoo :: a }
-- > newtype Bar a = MkBar { getBar :: a }
-- > newtype Baz a = MkBaz { getBaz :: a }
-- > f :: Baz Int -> Bar (Foo String)
--
-- makes it possible to write:
--
-- > x :: Int -> String
-- > x = coerce f
--
-- instead of
--
-- > x = getFoo . getBar . f . MkBaz

------------------

-- $identity
--
-- #identity#
-- The simplest Applicative functor is 'Identity', which just wraps and unwraps
-- pure values and function application.  This allows us to define
-- 'fmapDefault':
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > import Data.Coercible (coerce)
-- >
-- > fmapDefault :: forall t a b. Traversable t => (a -> b) -> t a -> t b
-- > fmapDefault = coerce (traverse @t @Identity @a @b)
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap terms via 'Identity' and 'runIdentity'.
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
-- passed to its right argument, which returns the final state.
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
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > mapAccumL :: forall t s a b. Traversable t
-- >           => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- > mapAccumL g s ts = coerce (traverse @t @(StateL s) @a @b) (flip g) ts s
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@newtype@__ terms.
--
-- The type of __@flip g@__ is coercible to __@a -> StateL b@__, which makes it
-- suitable for use with 'traverse'.  As part of the Applicative
-- [construction](#construction) of __@StateL (t b)@__ the state updates will
-- thread left-to-right along the sequence of elements of __@t a@__.
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
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > mapAccumR :: forall t s a b. Traversable t
-- >           => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
-- > mapAccumR g s0 ts = coerce (traverse @t @(StateR s) @a @b) (flip g) ts s0
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@newtype@__ terms.
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
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@newtype@__ terms.
--
-- We can therefore define a specialisation of 'traverse':
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > traverseC :: forall t a m. (Monoid m, Traversable t)
-- >           => (a -> Const m ()) -> t a -> Const m (t ())
-- > traverseC = traverse @t @(Const m) @a @()
--
-- For which the Applicative [construction](#construction) of 'traverse'
-- leads to:
--
-- prop> null ts ==> traverseC g ts = Const mempty
-- prop> traverseC g (prepend x xs) = Const (g x) <> traverseC g xs
--
-- In other words, this makes it possible to define:
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > foldMapDefault :: forall t a m. (Monoid m, Traversable t) => (a -> m) -> t a -> m
-- > foldMapDefault = coerce (traverse @t @(Const m) @a @())
--
-- Which is sufficient to define a 'Foldable' superclass instance:
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@newtype@__ terms.
--
-- > instance Traversable t => Foldable t where foldMap = foldMapDefault
--
-- It may however be instructive to also directly define candidate default
-- implementations of 'foldr' and 'foldl'', which take a bit more machinery
-- to construct:
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- > import Data.Coerce (coerce)
-- > import Data.Functor.Const (Const(..))
-- > import Data.Semigroup (Dual(..), Endo(..))
-- > import GHC.Exts (oneShot)
-- >
-- > foldrDefault :: forall t a b. Traversable t
-- >              => (a -> b -> b) -> b -> t a -> b
-- > foldrDefault f z = \t ->
-- >     coerce (traverse @t @(Const (Endo b)) @a @()) f t z
-- >
-- > foldlDefault' :: forall t a b. Traversable t => (b -> a -> b) -> b -> t a -> b
-- > foldlDefault' f z = \t ->
-- >     coerce (traverse @t @(Const (Dual (Endo b))) @a @()) f' t z
-- >   where
-- >     f' :: a -> b -> b
-- >     f' a = oneShot $ \ b -> b `seq` f b a
--
-- In the above we're using the __@'Data.Monoid.Endo' b@__ 'Monoid' and its
-- 'Dual' to compose a sequence of __@b -> b@__ accumulator updates in either
-- left-to-right or right-to-left order.
--
-- The use of 'seq' in the definition of __@foldlDefault'@__ ensures strictness
-- in the accumulator.
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@newtype@__ terms.
--
-- The 'GHC.Exts.oneShot' function gives a hint to the compiler that aids in
-- correct optimisation of lambda terms that fire at most once (for each
-- element __@a@__) and so should not try to pre-compute and re-use
-- subexpressions that pay off only on repeated execution.  Otherwise, it is
-- just the identity function.

------------------

-- $ziplist
--
-- #ziplist#
-- As a warm-up for looking at the 'ZipList' 'Applicative' functor, we'll first
-- look at a simpler analogue.  First define a fixed width 2-element @Vec2@
-- type, whose 'Applicative' instance combines a pair of functions with a pair of
-- values by applying each function to the corresponding value slot:
--
-- > data Vec2 a = Vec2 a a
-- > instance Functor Vec2 where
-- >     fmap f (Vec2 a b) = Vec2 (f a) (f b)
-- > instance Applicative Vec2 where
-- >     pure x = Vec2 x x
-- >     liftA2 f (Vec2 a b) (Vec2 p q) = Vec2 (f a p) (f b q)
-- > instance Foldable Vec2 where
-- >     foldr f z (Vec2 a b) = f a (f b z)
-- >     foldMap f (Vec2 a b) = f a <> f b
-- > instance Traversable Vec2 where
-- >     traverse f (Vec2 a b) = Vec2 <$> f a <*> f b
--
-- Along with a similar definition for fixed width 3-element vectors:
--
-- > data Vec3 a = Vec3 a a a
-- > instance Functor Vec3 where
-- >     fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
-- > instance Applicative Vec3 where
-- >     pure x = Vec3 x x x
-- >     liftA2 f (Vec3 p q r) (Vec3 x y z) = Vec3 (f p x) (f q y) (f r z)
-- > instance Foldable Vec3 where
-- >     foldr f z (Vec3 a b c) = f a (f b (f c z))
-- >     foldMap f (Vec3 a b c) = f a <> f b <> f c
-- > instance Traversable Vec3 where
-- >     traverse f (Vec3 a b c) = Vec3 <$> f a <*> f b <*> f c
--
-- With the above definitions, @'sequenceA'@ (same as @'traverse' 'id'@) acts
-- as a /matrix transpose/ operation on @Vec2 (Vec3 Int)@ producing a
-- corresponding @Vec3 (Vec2 Int)@:
--
-- Let __@t = Vec2 (Vec3 1 2 3) (Vec3 4 5 6)@__ be our 'Traversable' structure,
-- and __@g = id :: Vec3 Int -> Vec3 Int@__ be the function used to traverse
-- __@t@__.  We then have:
--
-- > traverse g t = Vec2 <$> (Vec3 1 2 3) <*> (Vec3 4 5 6)
-- >              = Vec3 (Vec2 1 4) (Vec2 2 5) (Vec2 3 6)
--
-- This construction can be generalised from fixed width vectors to variable
-- length lists via 'Control.Applicative.ZipList'.  This gives a transpose
-- operation that works well for lists of equal length.  If some of the lists
-- are longer than others, they're truncated to the longest common length.
--
-- We've already looked at the standard 'Applicative' instance of @List@ for
-- which applying __@m@__ functions __@f1, f2, ..., fm@__ to __@n@__ input
-- values __@a1, a2, ..., an@__ produces __@m * n@__ outputs:
--
-- >>> :set -XTupleSections
-- >>> [("f1",), ("f2",), ("f3",)] <*> [1,2]
-- [("f1",1),("f1",2),("f2",1),("f2",2),("f3",1),("f3",2)]
--
-- There are however two more common ways to turn lists into 'Applicative'
-- control structures.  The first is via __@'Const' [a]@__, since lists are
-- monoids under concatenation, and we've already seen that __@'Const' m@__ is
-- an 'Applicative' functor when __@m@__ is a 'Monoid'.  The second, is based
-- on 'Data.List.zipWith', and is called 'Control.Applicative.ZipList':
--
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > newtype ZipList a = ZipList { getZipList :: [a] }
-- >     deriving (Show, Eq, ..., Functor)
-- >
-- > instance Applicative ZipList where
-- >     liftA2 f (ZipList xs) (ZipList ys) = ZipList $ zipWith f xs ys
-- >     pure x = repeat x
--
-- The 'liftA2' definition is clear enough, instead of applying __@f@__ to each
-- pair __@(x, y)@__ drawn independently from the __@xs@__ and __@ys@__, only
-- corresponding pairs at each index in the two lists are used.
--
-- The definition of 'pure' may look surprising, but it is needed to ensure
-- that the instance is lawful:
--
-- prop> liftA2 f (pure x) ys == fmap (f x) ys
--
-- Since __@ys@__ can have any length, we need to provide an infinite supply
-- of __@x@__ values in __@pure x@__ in order to have a value to pair with
-- each element __@y@__.
--
-- When 'Control.Applicative.ZipList' is the 'Applicative' functor used in the
-- [construction](#construction) of a traversal, a ZipList holding a partially
-- built structure with __@m@__ elements is combined with a component holding
-- __@n@__ elements via 'zipWith', resulting in __@min m n@__ outputs!
--
-- Therefore 'traverse' with __@g :: a -> ZipList b@__ will produce a @ZipList@
-- of __@t b@__ structures whose element count is the minimum length of the
-- ZipLists __@g a@__ with __@a@__ ranging over the elements of __@t@__.  When
-- __@t@__ is empty, the length is infinite (as expected for a minimum of an
-- empty set).
--
-- If the structure __@t@__ holds values of type __@ZipList a@__, we can use
-- the identity function __@id :: ZipList a -> ZipList a@__ for the first
-- argument of 'traverse':
--
-- > traverse (id :: ZipList a -> ZipList a) :: t (ZipList a) -> ZipList (t a)
--
-- The number of elements in the output @ZipList@ will be the length of the
-- shortest @ZipList@ element of __@t@__.  Each output __@t a@__ will have the
-- /same shape/ as the input __@t (ZipList a)@__, i.e. will share its number of
-- elements.
--
-- If we think of the elements of __@t (ZipList a)@__ as its rows, and the
-- elements of each individual @ZipList@ as the columns of that row, we see
-- that our traversal implements a /transpose/ operation swapping the rows
-- and columns of __@t@__, after first truncating all the rows to the column
-- count of the shortest one.
--
-- Since in fact __@'traverse' id@__ is just 'sequenceA' the above boils down
-- to a rather concise definition of /transpose/, with [coercion](#coercion)
-- used to implicily wrap and unwrap the @ZipList@ @newtype@ as neeed, giving
-- a function that operates on a list of lists:
--
-- >>> {-# LANGUAGE ScopedTypeVariables #-}
-- >>> import Control.Applicative (ZipList(..))
-- >>> import Data.Coerce (coerce)
-- >>>
-- >>> transpose :: forall a. [[a]] -> [[a]]
-- >>> transpose = coerce (sequenceA :: [ZipList a] -> ZipList [a])
-- >>>
-- >>> transpose [[1,2,3],[4..],[7..]]
-- [[1,4,7],[2,5,8],[3,6,9]]
--
-- The use of [coercion](#coercion) avoids the need to explicitly wrap and
-- unwrap __@ZipList@__ terms.

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
--  * \"The Essence of the Iterator Pattern\",
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
