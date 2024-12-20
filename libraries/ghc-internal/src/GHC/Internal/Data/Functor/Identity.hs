{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  stable
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.
--
-- @since base-4.8.0.0
-----------------------------------------------------------------------------

module GHC.Internal.Data.Functor.Identity (
    Identity(..),
  ) where

import GHC.Internal.Control.Monad.Fix
import GHC.Internal.Data.Bits (Bits, FiniteBits)
import GHC.Internal.Data.Coerce
import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Functor.Utils ((#.))
import GHC.Internal.Foreign.Storable (Storable)
import GHC.Internal.Ix (Ix)
import GHC.Internal.Base ( Applicative(..), Eq(..), Functor(..), Monad(..)
                , Semigroup, Monoid, Ord(..), ($), (.) )
import GHC.Internal.Enum (Bounded, Enum)
import GHC.Internal.Float (Floating, RealFloat)
import GHC.Internal.Generics (Generic, Generic1)
import GHC.Internal.Num (Num)
import GHC.Internal.Read (Read(..), lex, readParen)
import GHC.Internal.Real (Fractional, Integral, Real, RealFrac)
import GHC.Internal.Show (Show(..), showParen, showString)
import GHC.Internal.Types (Bool(..))

-- | Identity functor and monad. (a non-strict monad)
--
-- ==== __Examples__
--
-- >>> fmap (+1) (Identity 0)
-- Identity 1
--
-- >>> Identity [1, 2, 3] <> Identity [4, 5, 6]
-- Identity [1,2,3,4,5,6]
--
-- @
-- >>> do
--       x <- Identity 10
--       y <- Identity (x + 5)
--       pure (x + y)
-- Identity 25
-- @
--
-- @since base-4.8.0.0
newtype Identity a = Identity { runIdentity :: a }
    deriving ( Bits       -- ^ @since base-4.9.0.0
             , Bounded    -- ^ @since base-4.9.0.0
             , Enum       -- ^ @since base-4.9.0.0
             , Eq         -- ^ @since base-4.8.0.0
             , FiniteBits -- ^ @since base-4.9.0.0
             , Floating   -- ^ @since base-4.9.0.0
             , Fractional -- ^ @since base-4.9.0.0
             , Generic    -- ^ @since base-4.8.0.0
             , Generic1   -- ^ @since base-4.8.0.0
             , Integral   -- ^ @since base-4.9.0.0
             , Ix         -- ^ @since base-4.9.0.0
             , Semigroup  -- ^ @since base-4.9.0.0
             , Monoid     -- ^ @since base-4.9.0.0
             , Num        -- ^ @since base-4.9.0.0
             , Ord        -- ^ @since base-4.8.0.0
             , Real       -- ^ @since base-4.9.0.0
             , RealFrac   -- ^ @since base-4.9.0.0
             , RealFloat  -- ^ @since base-4.9.0.0
             , Storable   -- ^ @since base-4.9.0.0
             )

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
--
-- @since base-4.8.0.0
instance (Read a) => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
--
-- @since base-4.8.0.0
instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
        showString "Identity " . showsPrec 11 x

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

-- | @since base-4.8.0.0
instance Foldable Identity where
    foldMap                = coerce

    elem                   = (. runIdentity) #. (==)
    foldl                  = coerce
    foldl'                 = coerce
    foldl1 _               = runIdentity
    foldr f z (Identity x) = f x z
    foldr'                 = foldr
    foldr1 _               = runIdentity
    length _               = 1
    maximum                = runIdentity
    minimum                = runIdentity
    null _                 = False
    product                = runIdentity
    sum                    = runIdentity
    toList (Identity x)    = [x]

-- | @since base-4.8.0.0
instance Functor Identity where
    fmap     = coerce

-- | @since base-4.8.0.0
instance Applicative Identity where
    pure     = Identity
    (<*>)    = coerce
    liftA2   = coerce

-- | @since base-4.8.0.0
instance Monad Identity where
    m >>= k  = k (runIdentity m)

-- | @since base-4.8.0.0
instance MonadFix Identity where
    mfix f   = Identity (fix (runIdentity . f))
