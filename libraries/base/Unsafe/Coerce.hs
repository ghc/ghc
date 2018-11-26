{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

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
--     'unsafeCoerce#' in "GHC.Base" (around which 'unsafeCoerce' is just a
--     trivial wrapper).
--
--   * In nhc98, the only representation-safe coercions are between
--     'Prelude.Enum' types with the same range (e.g. 'Prelude.Int',
--     'Data.Int.Int32', 'Prelude.Char', 'Data.Word.Word32'), or between a
--     newtype and the type that it wraps.
--
-----------------------------------------------------------------------------

module Unsafe.Coerce (unsafeCoerce) where

import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import GHC.Natural () -- See Note [Depend on GHC.Natural] in GHC.Base
import GHC.Prim (unsafeCoerce#)

local_id :: a -> a
local_id x = x   -- See Note [Mega-hack for coerce]

{- Note [Mega-hack for coerce]

If we just say
  unsafeCoerce x = unsafeCoerce# x
then the simple-optimiser that the desugarer runs will eta-reduce to
  unsafeCoerce :: forall (a:*) (b:*). a -> b
  unsafeCoerce = unsafeCoerce#
But we shouldn't be calling unsafeCoerce# in a higher
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
  -- NB: Do not eta-reduce this definition (see above)
