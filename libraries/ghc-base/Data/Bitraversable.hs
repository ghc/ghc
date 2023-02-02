{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bitraversable
-- Copyright   :  (C) 2011-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- @since 4.10.0.0
----------------------------------------------------------------------------
module Data.Bitraversable
  ( Bitraversable(..)
  , bisequenceA
  , bisequence
  , bimapM
  , bifor
  , biforM
  , bimapAccumL
  , bimapAccumR
  , bimapDefault
  , bifoldMapDefault
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Coerce
import Data.Functor.Identity (Identity(..))
import Data.Functor.Utils (StateL(..), StateR(..))
import GHC.Generics (K1(..))

-- $setup
-- >>> import Prelude
-- >>> import Data.Maybe
-- >>> import Data.List (find)

-- | 'Bitraversable' identifies bifunctorial data structures whose elements can
-- be traversed in order, performing 'Applicative' or 'Monad' actions at each
-- element, and collecting a result structure with the same shape.
--
-- As opposed to 'Traversable' data structures, which have one variety of
-- element on which an action can be performed, 'Bitraversable' data structures
-- have two such varieties of elements.
--
-- A definition of 'bitraverse' must satisfy the following laws:
--
-- [Naturality]
--   @'bitraverse' (t . f) (t . g) ≡ t . 'bitraverse' f g@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'bitraverse' 'Identity' 'Identity' ≡ 'Identity'@
--
-- [Composition]
--   @'Data.Functor.Compose.Compose' .
--    'fmap' ('bitraverse' g1 g2) .
--    'bitraverse' f1 f2
--     ≡ 'bitraverse' ('Data.Functor.Compose.Compose' . 'fmap' g1 . f1)
--                  ('Data.Functor.Compose.Compose' . 'fmap' g2 . f2)@
--
-- where an /applicative transformation/ is a function
--
-- @t :: ('Applicative' f, 'Applicative' g) => f a -> g a@
--
-- preserving the 'Applicative' operations:
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
-- Some simple examples are 'Either' and @(,)@:
--
-- > instance Bitraversable Either where
-- >   bitraverse f _ (Left x) = Left <$> f x
-- >   bitraverse _ g (Right y) = Right <$> g y
-- >
-- > instance Bitraversable (,) where
-- >   bitraverse f g (x, y) = (,) <$> f x <*> g y
--
-- 'Bitraversable' relates to its superclasses in the following ways:
--
-- @
-- 'bimap' f g ≡ 'runIdentity' . 'bitraverse' ('Identity' . f) ('Identity' . g)
-- 'bifoldMap' f g = 'getConst' . 'bitraverse' ('Const' . f) ('Const' . g)
-- @
--
-- These are available as 'bimapDefault' and 'bifoldMapDefault' respectively.
--
-- @since 4.10.0.0
class (Bifunctor t, Bifoldable t) => Bitraversable t where
  -- | Evaluates the relevant functions at each element in the structure,
  -- running the action, and builds a new structure with the same shape, using
  -- the results produced from sequencing the actions.
  --
  -- @'bitraverse' f g ≡ 'bisequenceA' . 'bimap' f g@
  --
  -- For a version that ignores the results, see 'bitraverse_'.
  --
  -- ==== __Examples__
  --
  -- Basic usage:
  --
  -- >>> bitraverse listToMaybe (find odd) (Left [])
  -- Nothing
  --
  -- >>> bitraverse listToMaybe (find odd) (Left [1, 2, 3])
  -- Just (Left 1)
  --
  -- >>> bitraverse listToMaybe (find odd) (Right [4, 5])
  -- Just (Right 5)
  --
  -- >>> bitraverse listToMaybe (find odd) ([1, 2, 3], [4, 5])
  -- Just (1,5)
  --
  -- >>> bitraverse listToMaybe (find odd) ([], [4, 5])
  -- Nothing
  --
  -- @since 4.10.0.0
  bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

-- | Alias for 'bisequence'.
--
-- @since 4.10.0.0
bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = bisequence

-- | Alias for 'bitraverse'.
--
-- @since 4.10.0.0
bimapM :: (Bitraversable t, Applicative f)
       => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = bitraverse

-- | Sequences all the actions in a structure, building a new structure with
-- the same shape using the results of the actions. For a version that ignores
-- the results, see 'bisequence_'.
--
-- @'bisequence' ≡ 'bitraverse' 'id' 'id'@
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bisequence (Just 4, Nothing)
-- Nothing
--
-- >>> bisequence (Just 4, Just 5)
-- Just (4,5)
--
-- >>> bisequence ([1, 2, 3], [4, 5])
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
--
-- @since 4.10.0.0
bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = bitraverse id id

-- | @since 4.10.0.0
instance Bitraversable (,) where
  bitraverse f g ~(a, b) = liftA2 (,) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable ((,,) x) where
  bitraverse f g ~(x, a, b) = liftA2 ((,,) x) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable ((,,,) x y) where
  bitraverse f g ~(x, y, a, b) = liftA2 ((,,,) x y) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable ((,,,,) x y z) where
  bitraverse f g ~(x, y, z, a, b) = liftA2 ((,,,,) x y z) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable ((,,,,,) x y z w) where
  bitraverse f g ~(x, y, z, w, a, b) = liftA2 ((,,,,,) x y z w) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable ((,,,,,,) x y z w v) where
  bitraverse f g ~(x, y, z, w, v, a, b) =
    liftA2 ((,,,,,,) x y z w v) (f a) (g b)

-- | @since 4.10.0.0
instance Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b

-- | @since 4.10.0.0
instance Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a

-- | @since 4.10.0.0
instance Bitraversable (K1 i) where
  bitraverse f _ (K1 c) = K1 <$> f c

-- | 'bifor' is 'bitraverse' with the structure as the first argument. For a
-- version that ignores the results, see 'bifor_'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bifor (Left []) listToMaybe (find even)
-- Nothing
--
-- >>> bifor (Left [1, 2, 3]) listToMaybe (find even)
-- Just (Left 1)
--
-- >>> bifor (Right [4, 5]) listToMaybe (find even)
-- Just (Right 4)
--
-- >>> bifor ([1, 2, 3], [4, 5]) listToMaybe (find even)
-- Just (1,4)
--
-- >>> bifor ([], [4, 5]) listToMaybe (find even)
-- Nothing
--
-- @since 4.10.0.0
bifor :: (Bitraversable t, Applicative f)
      => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t

-- | Alias for 'bifor'.
--
-- @since 4.10.0.0
biforM :: (Bitraversable t, Applicative f)
       => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = bifor

-- | The 'bimapAccumL' function behaves like a combination of 'bimap' and
-- 'bifoldl'; it traverses a structure from left to right, threading a state
-- of type @a@ and using the given actions to compute new elements for the
-- structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bimapAccumL (\acc bool -> (acc + 1, show bool)) (\acc string -> (acc * 2, reverse string)) 3 (True, "foo")
-- (8,("True","oof"))
--
-- @since 4.10.0.0
bimapAccumL :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e))
            -> a -> t b d -> (a, t c e)
bimapAccumL f g s t
  = runStateL (bitraverse (StateL . flip f) (StateL . flip g) t) s

-- | The 'bimapAccumR' function behaves like a combination of 'bimap' and
-- 'bifoldr'; it traverses a structure from right to left, threading a state
-- of type @a@ and using the given actions to compute new elements for the
-- structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bimapAccumR (\acc bool -> (acc + 1, show bool)) (\acc string -> (acc * 2, reverse string)) 3 (True, "foo")
-- (7,("True","oof"))
--
-- @since 4.10.0.0
bimapAccumR :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e))
            -> a -> t b d -> (a, t c e)
bimapAccumR f g s t
  = runStateR (bitraverse (StateR . flip f) (StateR . flip g) t) s

-- | A default definition of 'bimap' in terms of the 'Bitraversable'
-- operations.
--
-- @'bimapDefault' f g ≡
--     'runIdentity' . 'bitraverse' ('Identity' . f) ('Identity' . g)@
--
-- @since 4.10.0.0
bimapDefault :: forall t a b c d . Bitraversable t
             => (a -> b) -> (c -> d) -> t a c -> t b d
-- See Note [Function coercion] in Data.Functor.Utils.
bimapDefault = coerce
  (bitraverse :: (a -> Identity b)
              -> (c -> Identity d) -> t a c -> Identity (t b d))
{-# INLINE bimapDefault #-}

-- | A default definition of 'bifoldMap' in terms of the 'Bitraversable'
-- operations.
--
-- @'bifoldMapDefault' f g ≡
--    'getConst' . 'bitraverse' ('Const' . f) ('Const' . g)@
--
-- @since 4.10.0.0
bifoldMapDefault :: forall t m a b . (Bitraversable t, Monoid m)
                 => (a -> m) -> (b -> m) -> t a b -> m
-- See Note [Function coercion] in Data.Functor.Utils.
bifoldMapDefault = coerce
  (bitraverse :: (a -> Const m ())
              -> (b -> Const m ()) -> t a b -> Const m (t () ()))
{-# INLINE bifoldMapDefault #-}
