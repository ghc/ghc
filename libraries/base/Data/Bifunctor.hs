{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor
-- Copyright   :  (C) 2008-2014 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- @since 4.8.0.0
----------------------------------------------------------------------------
module Data.Bifunctor
  ( Bifunctor(..)
  ) where

import Control.Applicative  ( Const(..) )
import GHC.Generics ( K1(..) )

-- $setup
-- >>> import Prelude
-- >>> import Data.Char (toUpper)

-- | A bifunctor is a type constructor that takes
-- two type arguments and is a functor in /both/ arguments. That
-- is, unlike with 'Functor', a type constructor such as 'Either'
-- does not need to be partially applied for a 'Bifunctor'
-- instance, and the methods in this class permit mapping
-- functions over the 'Left' value or the 'Right' value,
-- or both at the same time.
--
-- Formally, the class 'Bifunctor' represents a bifunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where both the first and second
-- arguments are covariant.
--
-- You can define a 'Bifunctor' by either defining 'bimap' or by
-- defining both 'first' and 'second'.
--
-- If you supply 'bimap', you should ensure that:
--
-- @'bimap' 'id' 'id' ≡ 'id'@
--
-- If you supply 'first' and 'second', ensure:
--
-- @
-- 'first' 'id' ≡ 'id'
-- 'second' 'id' ≡ 'id'
-- @
--
-- If you supply both, you should also ensure:
--
-- @'bimap' f g ≡ 'first' f '.' 'second' g@
--
-- These ensure by parametricity:
--
-- @
-- 'bimap'  (f '.' g) (h '.' i) ≡ 'bimap' f h '.' 'bimap' g i
-- 'first'  (f '.' g) ≡ 'first'  f '.' 'first'  g
-- 'second' (f '.' g) ≡ 'second' f '.' 'second' g
-- @
--
-- @since 4.8.0.0
class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    -- | Map over both arguments at the same time.
    --
    -- @'bimap' f g ≡ 'first' f '.' 'second' g@
    --
    -- ==== __Examples__
    -- >>> bimap toUpper (+1) ('j', 3)
    -- ('J',4)
    --
    -- >>> bimap toUpper (+1) (Left 'j')
    -- Left 'J'
    --
    -- >>> bimap toUpper (+1) (Right 3)
    -- Right 4
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g


    -- | Map covariantly over the first argument.
    --
    -- @'first' f ≡ 'bimap' f 'id'@
    --
    -- ==== __Examples__
    -- >>> first toUpper ('j', 3)
    -- ('J',3)
    --
    -- >>> first toUpper (Left 'j')
    -- Left 'J'
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id


    -- | Map covariantly over the second argument.
    --
    -- @'second' ≡ 'bimap' 'id'@
    --
    -- ==== __Examples__
    -- >>> second (+1) ('j', 3)
    -- ('j',4)
    --
    -- >>> second (+1) (Right 3)
    -- Right 4
    second :: (b -> c) -> p a b -> p a c
    second = bimap id



-- | @since 4.8.0.0
instance Bifunctor (,) where
    bimap f g ~(a, b) = (f a, g b)

-- | @since 4.8.0.0
instance Bifunctor ((,,) x1) where
    bimap f g ~(x1, a, b) = (x1, f a, g b)

-- | @since 4.8.0.0
instance Bifunctor ((,,,) x1 x2) where
    bimap f g ~(x1, x2, a, b) = (x1, x2, f a, g b)

-- | @since 4.8.0.0
instance Bifunctor ((,,,,) x1 x2 x3) where
    bimap f g ~(x1, x2, x3, a, b) = (x1, x2, x3, f a, g b)

-- | @since 4.8.0.0
instance Bifunctor ((,,,,,) x1 x2 x3 x4) where
    bimap f g ~(x1, x2, x3, x4, a, b) = (x1, x2, x3, x4, f a, g b)

-- | @since 4.8.0.0
instance Bifunctor ((,,,,,,) x1 x2 x3 x4 x5) where
    bimap f g ~(x1, x2, x3, x4, x5, a, b) = (x1, x2, x3, x4, x5, f a, g b)


-- | @since 4.8.0.0
instance Bifunctor Either where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right b) = Right (g b)

-- | @since 4.8.0.0
instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

-- | @since 4.9.0.0
instance Bifunctor (K1 i) where
    bimap f _ (K1 c) = K1 (f c)
