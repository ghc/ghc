{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor
  ( -- * Overview
    --
    -- Bifunctors extend the standard 'Functor' to two arguments

    -- * Examples
    -- $examples
    Bifunctor(..)
  ) where

import Control.Applicative
import Data.Functor.Constant
import Data.Semigroup

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (K1(..))
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

-- | Minimal definition either 'bimap' or 'first' and 'second'

-- | Formally, the class 'Bifunctor' represents a bifunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where both the first and second arguments are covariant.
--
-- You can define a 'Bifunctor' by either defining 'bimap' or by defining both
-- 'first' and 'second'.
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
class Bifunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'bimap' f g ≡ 'first' f '.' 'second' g@
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  {-# INLINE bimap #-}

  -- | Map covariantly over the first argument.
  --
  -- @'first' f ≡ 'bimap' f 'id'@
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}

  -- | Map covariantly over the second argument.
  --
  -- @'second' ≡ 'bimap' 'id'@
  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bimap | first, second #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Bifunctor
#endif

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor Arg where
  bimap f g (Arg a b) = Arg (f a) (g b)

instance Bifunctor ((,,) x) where
  bimap f g ~(x, a, b) = (x, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,) x y) where
  bimap f g ~(x, y, a, b) = (x, y, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,,) x y z) where
  bimap f g ~(x, y, z, a, b) = (x, y, z, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,,,) x y z w) where
  bimap f g ~(x, y, z, w, a, b) = (x, y, z, w, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,,,,) x y z w v) where
  bimap f g ~(x, y, z, w, v, a, b) = (x, y, z, w, v, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
  {-# INLINE bimap #-}

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  {-# INLINE bimap #-}

instance Bifunctor Constant where
  bimap f _ (Constant a) = Constant (f a)
  {-# INLINE bimap #-}

#if __GLASGOW_HASKELL__ >= 702
instance Bifunctor (K1 i) where
  bimap f _ (K1 c) = K1 (f c)
  {-# INLINE bimap #-}
#endif

#ifdef MIN_VERSION_tagged
instance Bifunctor Tagged where
  bimap _ g (Tagged b) = Tagged (g b)
  {-# INLINE bimap #-}
#endif

-- $examples
--
-- ==== __Examples__
--
-- While the standard 'Functor' instance for 'Either' is limited to mapping over 'Right' arguments,
-- the 'Bifunctor' instance allows mapping over the 'Left', 'Right', or both arguments:
--
-- > let x = Left "foo" :: Either String Integer
--
-- In the case of 'first' and 'second', the function may or may not be applied:
--
-- > first (++ "bar") x == Left "foobar"
-- > second (+2) x      == Left "foo"
--
-- In the case of 'bimap', only one of the functions will be applied:
--
-- > bimap (++ "bar") (+2) x == Left "foobar"
--
-- The 'Bifunctor' instance for 2 element tuples allows mapping over one or both of the elements:
--
-- > let x = ("foo",1)
-- >
-- > first  (++ "bar") x      == ("foobar", 1)
-- > second (+2) x            == ("foo", 3)
-- > bimap  (++ "bar") (+2) x == ("foobar", 3)
