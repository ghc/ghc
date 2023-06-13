{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
DO NOT USE THIS MODULE.  Use "GHC.TypeNats" instead.

This module is internal-only and was exposed by accident.  It may be
removed without warning in a future version.

(The technical reason for this module's existence is that it is needed
to prevent module cycles while still allowing these identifiers to be
imported in 'Data.Type.Ord'.)

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
type family CmpNat (m :: Natural) (n :: Natural) :: Ordering
