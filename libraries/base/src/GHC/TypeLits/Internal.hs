{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.TypeLits.Internal
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- __Do not use this module.__  Use "GHC.TypeLits" instead.
--
-- This module is internal-only and was exposed by accident.  It may be
-- removed without warning in a future version.
--
-- /The API of this module is unstable and is tightly coupled to GHC's internals./
-- If depend on it, make sure to use a tight upper bound, e.g., @base < 4.X@ rather
-- than @base < 5@, because the interface can change rapidly without much warning.
--
-- The technical reason for this module's existence is that it is needed
-- to prevent module cycles while still allowing these identifiers to be
-- imported in "Data.Type.Ord".
--
-- @since 4.16.0.0

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
