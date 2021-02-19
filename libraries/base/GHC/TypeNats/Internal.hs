{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
This module exports the Type Nat kind as well as the comparison type
family for that kinds.  It is needed to prevent module cycles while still
allowing these identifiers to be improted in 'Data.Type.Ord'.

@since 4.16.0.0
-}

module GHC.TypeNats.Internal
  ( Nat
  , CmpNat
  ) where

import GHC.Base(Ordering)
import GHC.Num.Natural(Natural)

-- | A type synonym for 'Natural'.
--
-- Prevously, this was an opaque data type, but it was changed to a type synonym
-- @since @base-4.15.0.0@.

type Nat = Natural

-- | Comparison of type-level naturals, as a function.
--
-- @since 4.7.0.0
type family CmpNat (m :: Nat) (n :: Nat) :: Ordering
