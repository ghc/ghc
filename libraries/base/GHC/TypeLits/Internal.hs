{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
This module exports the Type Literal kinds as well as the comparison type
families for those kinds.  It is needed to prevent module cycles while still
allowing these identifiers to be improted in 'Data.Type.Ord'.

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
type CmpSymbol :: Symbol -> Symbol -> Ordering
type family CmpSymbol m n

-- Char-related type families

-- | Comparison of type-level characters.
--
-- @since 4.16.0.0
type CmpChar :: Char -> Char -> Ordering
type family CmpChar a b
