{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
DO NOT USE THIS MODULE.  Use "GHC.TypeLits" instead.

This module is internal-only and was exposed by accident.  It may be
removed without warning in a future version.

(The technical reason for this module's existence is that it is needed
to prevent module cycles while still allowing these identifiers to be
imported in 'Data.Type.Ord'.)

@since 4.16.0.0
-}

module GHC.TypeLits.Internal
  ( Symbol
  , CmpSymbol, CmpChar
  ) where

import GHC.Base(Ordering)
import GHC.Types(Symbol, Char)

-- | Comparison of type-level symbols, as a function.
--
-- @since 4.7.0.0
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

-- Char-related type families

-- | Comparison of type-level characters.
--
-- @since 4.16.0.0
type family CmpChar (a :: Char) (b :: Char) :: Ordering
