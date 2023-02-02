{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Identity
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
-- @since 4.8.0.0
-----------------------------------------------------------------------------

module Data.Functor.Identity (
    Identity(..),
  ) where

import Control.Monad.Fix
import Data.Bits (Bits, FiniteBits)
import Data.Coerce
import Data.Foldable
import Data.Functor.Utils ((#.))
import Foreign.Storable (Storable)
import GHC.Ix (Ix)
import GHC.Base ( Applicative(..), Eq(..), Functor(..), Monad(..)
                , Semigroup, Monoid, Ord(..), ($), (.) )
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Read (Read(..), lex, readParen)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Show (Show(..), showParen, showString)
import GHC.Types (Bool(..))

-- | Identity functor and monad. (a non-strict monad)
--
-- @since 4.8.0.0
newtype Identity a = Identity { runIdentity :: a }
    deriving ( Bits       -- ^ @since 4.9.0.0
             , Bounded    -- ^ @since 4.9.0.0
             , Enum       -- ^ @since 4.9.0.0
             , Eq         -- ^ @since 4.8.0.0
             , FiniteBits -- ^ @since 4.9.0.0
             , Floating   -- ^ @since 4.9.0.0
             , Fractional -- ^ @since 4.9.0.0
             , Generic    -- ^ @since 4.8.0.0
             , Generic1   -- ^ @since 4.8.0.0
             , Integral   -- ^ @since 4.9.0.0
             , Ix         -- ^ @since 4.9.0.0
             , Semigroup  -- ^ @since 4.9.0.0
             , Monoid     -- ^ @since 4.9.0.0
             , Num        -- ^ @since 4.9.0.0
             , Ord        -- ^ @since 4.8.0.0
             , Real       -- ^ @since 4.9.0.0
             , RealFrac   -- ^ @since 4.9.0.0
             , RealFloat  -- ^ @since 4.9.0.0
             , Storable   -- ^ @since 4.9.0.0
             )

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
--
-- @since 4.8.0.0
instance (Read a) => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
--
-- @since 4.8.0.0
instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
        showString "Identity " . showsPrec 11 x

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

-- | @since 4.8.0.0
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

-- | @since 4.8.0.0
instance Functor Identity where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Identity where
    pure     = Identity
    (<*>)    = coerce
    liftA2   = coerce

-- | @since 4.8.0.0
instance Monad Identity where
    m >>= k  = k (runIdentity m)

-- | @since 4.8.0.0
instance MonadFix Identity where
    mfix f   = Identity (fix (runIdentity . f))
