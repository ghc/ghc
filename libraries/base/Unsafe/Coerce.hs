{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Unsafe.Coerce
-- Copyright   :  Malcolm Wallace 2006
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The highly unsafe primitive 'unsafeCoerce' converts a value from any
-- type to any other type.  Needless to say, if you use this function,
-- it is your responsibility to ensure that the old and new types have
-- identical internal representations, in order to prevent runtime corruption.
--
-- The types for which 'unsafeCoerce' is representation-safe may differ
-- from compiler to compiler (and version to version).
--
--   * Documentation for correct usage in GHC will be found under
--     'unsafeCoerce#' in GHC.Base (around which 'unsafeCoerce' is just a
--     trivial wrapper).
--
--   * In nhc98, the only representation-safe coercions are between Enum
--     types with the same range (e.g. Int, Int32, Char, Word32),
--     or between a newtype and the type that it wraps.
--
-----------------------------------------------------------------------------

module Unsafe.Coerce (unsafeCoerce) where


#if defined(__GLASGOW_HASKELL__)
import GHC.Integer () -- for build ordering
import GHC.Prim (unsafeCoerce#)

local_id :: a -> a
local_id x = x   -- See Note [Mega-hack for coerce]

{- Note [Meta-hack for coerce]

If we just say
  unsafeCoerce x = unsafeCoerce# x
then the simple-optimiser that the desugarer runs will eta-reduce to
  unsafeCoerce :: forall (a:*) (b:*). a -> b
  unsafeCoercs = unsafeCoerce#
And that, sadly, is ill-typed because unsafeCoercs# has OpenKind type variables
And rightly so, because we shouldn't be calling unsafeCoerce# in a higher
order way; it has a compulsory unfolding 
   unsafeCoerce# a b x = x |> UnsafeCo a b
and we really rely on it being inlined pronto. But the simple-optimiser doesn't.
The identity function local_id delays the eta reduction just long enough
for unsafeCoerce# to get inlined.

Sigh. This is horrible, but then so is unsafeCoerce.
-}

unsafeCoerce :: a -> b
unsafeCoerce x = local_id (unsafeCoerce# x)
  -- See Note [Unsafe coerce magic] in basicTypes/MkId
  -- NB: Do not eta-reduce this definition, else the type checker 
  -- give usafeCoerce the same (dangerous) type as unsafeCoerce#
#endif

#if defined(__HUGS__)
import Hugs.IOExts (unsafeCoerce)
#endif

