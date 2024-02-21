{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Functor.Const
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable

-- The 'Const' functor.
--
-- @since base-4.9.0.0

module GHC.Internal.Data.Functor.Const (Const(..)) where

import GHC.Internal.Data.Bits (Bits, FiniteBits)
import GHC.Internal.Data.Foldable (Foldable(foldMap))
import GHC.Internal.Foreign.Storable (Storable)

import GHC.Internal.Ix (Ix)
import GHC.Internal.Base
import GHC.Internal.Enum (Bounded, Enum)
import GHC.Internal.Float (Floating, RealFloat)
import GHC.Internal.Generics (Generic, Generic1)
import GHC.Internal.Num (Num)
import GHC.Internal.Real (Fractional, Integral, Real, RealFrac)
import GHC.Internal.Read (Read(readsPrec), readParen, lex)
import GHC.Internal.Show (Show(showsPrec), showParen, showString)

-- | The 'Const' functor.
--
-- ==== __Examples__
--
-- >>> fmap (++ "World") (Const "Hello")
-- Const "Hello"
--
-- Because we ignore the second type parameter to 'Const',
-- the Applicative instance, which has
-- @'(<*>)' :: Monoid m => Const m (a -> b) -> Const m a -> Const m b@
-- essentially turns into @Monoid m => m -> m -> m@, which is '(<>)'
--
-- >>> Const [1, 2, 3] <*> Const [4, 5, 6]
-- Const [1,2,3,4,5,6]
newtype Const a b = Const { getConst :: a }
    deriving ( Bits       -- ^ @since base-4.9.0.0
             , Bounded    -- ^ @since base-4.9.0.0
             , Enum       -- ^ @since base-4.9.0.0
             , Eq         -- ^ @since base-4.9.0.0
             , FiniteBits -- ^ @since base-4.9.0.0
             , Floating   -- ^ @since base-4.9.0.0
             , Fractional -- ^ @since base-4.9.0.0
             , Generic    -- ^ @since base-4.9.0.0
             , Generic1   -- ^ @since base-4.9.0.0
             , Integral   -- ^ @since base-4.9.0.0
             , Ix         -- ^ @since base-4.9.0.0
             , Semigroup  -- ^ @since base-4.9.0.0
             , Monoid     -- ^ @since base-4.9.0.0
             , Num        -- ^ @since base-4.9.0.0
             , Ord        -- ^ @since base-4.9.0.0
             , Real       -- ^ @since base-4.9.0.0
             , RealFrac   -- ^ @since base-4.9.0.0
             , RealFloat  -- ^ @since base-4.9.0.0
             , Storable   -- ^ @since base-4.9.0.0
             )

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'getConst' field were removed
--
-- @since base-4.8.0.0
instance Read a => Read (Const a b) where
    readsPrec d = readParen (d > 10)
        $ \r -> [(Const x,t) | ("Const", s) <- lex r, (x, t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'getConst' field were removed
--
-- @since base-4.8.0.0
instance Show a => Show (Const a b) where
    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x

-- | @since base-4.7.0.0
instance Foldable (Const m) where
    foldMap _ _ = mempty

-- | @since base-2.01
instance Functor (Const m) where
    fmap _ (Const v) = Const v

-- | @since base-2.0.1
instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    liftA2 _ (Const x) (Const y) = Const (x `mappend` y)
    (<*>) = coerce (mappend :: m -> m -> m)
-- This is pretty much the same as
-- Const f <*> Const v = Const (f `mappend` v)
-- but guarantees that mappend for Const a b will have the same arity
-- as the one for a; it won't create a closure to raise the arity
-- to 2.
