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
-- Module      :  GHC.Internal.Data.Traversable
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

module GHC.Internal.Data.Traversable (
    -- * The 'Traversable' class
    Traversable(..),
    -- * Utility functions
    for,
    forM,
    forAccumM,
    mapAccumL,
    mapAccumR,
    mapAccumM,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,
    ) where

import GHC.Internal.Data.Coerce
import GHC.Internal.Data.Either ( Either(..) )
import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Functor
import GHC.Internal.Data.Functor.Const ( Const(..) )
import GHC.Internal.Data.Functor.Identity ( Identity(..) )
import GHC.Internal.Data.Functor.Utils ( StateL(..), StateR(..), StateT(..), (#.) )
import GHC.Internal.Data.Monoid ( Dual(..), Sum(..), Product(..),
                     First(..), Last(..), Alt(..), Ap(..) )
import GHC.Internal.Data.Ord ( Down(..) )
import GHC.Internal.Data.Proxy ( Proxy(..) )

import GHC.Internal.Arr
import GHC.Internal.Base ( Applicative(..), Monad(..), Monoid, Maybe(..), NonEmpty(..),
                  ($), (.), id, flip )
import GHC.Internal.Generics
import qualified GHC.Internal.List as List ( foldr )
import GHC.Internal.Tuple (Solo (..))

-- $setup
-- >>> import Prelude
-- >>> import GHC.Internal.Data.Maybe (catMaybes, mapMaybe)
-- >>> import GHC.Internal.Data.Either (rights)
-- >>> import GHC.Internal.Data.Foldable (traverse_)

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

-- | @since base-2.01
instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

-- | @since base-2.01
instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

-- | @since base-4.9.0.0
instance Traversable NonEmpty where
  traverse f (a :| as) = liftA2 (:|) (f a) (traverse f as)

-- | @since base-4.7.0.0
instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

-- | @since base-4.15
deriving instance Traversable Solo

-- | @since base-4.7.0.0
instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

-- | @since base-2.01
instance Ix i => Traversable (Array i) where
    traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

-- | @since base-4.7.0.0
instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = pure Proxy
    {-# INLINE mapM #-}
    sequence _ = pure Proxy
    {-# INLINE sequence #-}

-- | @since base-4.7.0.0
instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

-- | @since base-4.8.0.0
instance Traversable Dual where
    traverse f (Dual x) = Dual <$> f x

-- | @since base-4.8.0.0
instance Traversable Sum where
    traverse f (Sum x) = Sum <$> f x

-- | @since base-4.8.0.0
instance Traversable Product where
    traverse f (Product x) = Product <$> f x

-- | @since base-4.8.0.0
instance Traversable First where
    traverse f (First x) = First <$> traverse f x

-- | @since base-4.8.0.0
instance Traversable Last where
    traverse f (Last x) = Last <$> traverse f x

-- | @since base-4.12.0.0
instance (Traversable f) => Traversable (Alt f) where
    traverse f (Alt x) = Alt <$> traverse f x

-- | @since base-4.12.0.0
instance (Traversable f) => Traversable (Ap f) where
    traverse f (Ap x) = Ap <$> traverse f x

-- | @since base-4.9.0.0
deriving instance Traversable Identity


-- Instances for GHC.Generics
-- | @since base-4.9.0.0
instance Traversable U1 where
    traverse _ _ = pure U1
    {-# INLINE traverse #-}
    sequenceA _ = pure U1
    {-# INLINE sequenceA #-}
    mapM _ _ = pure U1
    {-# INLINE mapM #-}
    sequence _ = pure U1
    {-# INLINE sequence #-}

-- | @since base-4.9.0.0
deriving instance Traversable V1

-- | @since base-4.9.0.0
deriving instance Traversable Par1

-- | @since base-4.9.0.0
deriving instance Traversable f => Traversable (Rec1 f)

-- | @since base-4.9.0.0
deriving instance Traversable (K1 i c)

-- | @since base-4.9.0.0
deriving instance Traversable f => Traversable (M1 i c f)

-- | @since base-4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

-- | @since base-4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)

-- | @since base-4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)

-- | @since base-4.9.0.0
deriving instance Traversable UAddr

-- | @since base-4.9.0.0
deriving instance Traversable UChar

-- | @since base-4.9.0.0
deriving instance Traversable UDouble

-- | @since base-4.9.0.0
deriving instance Traversable UFloat

-- | @since base-4.9.0.0
deriving instance Traversable UInt

-- | @since base-4.9.0.0
deriving instance Traversable UWord

-- Instance for Data.Ord
-- | @since base-4.12.0.0
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

-- | The `mapAccumM` function behaves like a combination of `mapM` and
-- `mapAccumL` that traverses the structure while evaluating the actions
-- and passing an accumulating parameter from left to right.
-- It returns a final value of this accumulator together with the new structure.
-- The accumulator is often used for caching the intermediate results of a computation.
--
--  @since base-4.18.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let expensiveDouble a = putStrLn ("Doubling " <> show a) >> pure (2 * a)
-- >>> :{
-- mapAccumM (\cache a -> case lookup a cache of
--     Nothing -> expensiveDouble a >>= \double -> pure ((a, double):cache, double)
--     Just double -> pure (cache, double)
--     ) [] [1, 2, 3, 1, 2, 3]
-- :}
-- Doubling 1
-- Doubling 2
-- Doubling 3
-- ([(3,6),(2,4),(1,2)],[2,4,6,2,4,6])
--
mapAccumM
  :: forall m t s a b. (Monad m, Traversable t)
  => (s -> a -> m (s, b))
  -> s -> t a -> m (s, t b)
mapAccumM f s t = coerce (mapM @t @(StateT s m) @a @b) (StateT #. flip f) t s

-- | 'forAccumM' is 'mapAccumM' with the arguments rearranged.
--
-- @since base-4.18.0.0
forAccumM
  :: (Monad m, Traversable t)
  => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
{-# INLINE forAccumM #-}
forAccumM s t f = mapAccumM f s t

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
