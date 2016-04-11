{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Const
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable

-- The 'Const' functor.
--
-- @since 4.9.0.0

module Data.Functor.Const (Const(..)) where

import Data.Bits (Bits, FiniteBits)
import Data.Foldable (Foldable(foldMap))
import Foreign.Storable (Storable)

import GHC.Arr (Ix)
import GHC.Base
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Read (Read(readsPrec), readParen, lex)
import GHC.Show (Show(showsPrec), showParen, showString)

-- | The 'Const' functor.
newtype Const a b = Const { getConst :: a }
    deriving ( Bits, Bounded, Enum, Eq, FiniteBits, Floating, Fractional
             , Generic, Generic1, Integral, Ix, Monoid, Num, Ord, Real
             , RealFrac, RealFloat , Storable)

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'runConst' field were removed
instance Read a => Read (Const a b) where
    readsPrec d = readParen (d > 10)
        $ \r -> [(Const x,t) | ("Const", s) <- lex r, (x, t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'runConst' field were removed
instance Show a => Show (Const a b) where
    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    (<*>) = coerce (mappend :: m -> m -> m)
-- This is pretty much the same as
-- Const f <*> Const v = Const (f `mappend` v)
-- but guarantees that mappend for Const a b will have the same arity
-- as the one for a; it won't create a closure to raise the arity
-- to 2.
