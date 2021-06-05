{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
This module exports the Type Nat kind as well as the comparison type
family for that kinds.  It is needed to prevent module cycles while still
allowing these identifiers to be improted in 'Data.Type.Ord'.

@since 4.16.0.0
-}

module GHC.TypeNats.Internal
  ( Natural
  , CmpNat
  ) where

import GHC.Base(Ordering)
import GHC.Num.Natural(Natural)

-- | Comparison of type-level naturals, as a function.
--
-- @since 4.7.0.0
type CmpNat :: Natural -> Natural -> Ordering
type family CmpNat m n
