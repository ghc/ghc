{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Primitive exceptions.
--
-- Users should not import this module.  It is GHC internal only.
module GHC.Prim.Exception
   ( raiseOverflow
   , raiseUnderflow
   , raiseDivZero
   )
where

import GHC.Prim

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Types ()

default () -- Double and Integer aren't available yet

-- Note [Arithmetic exceptions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ghc-prim provides several functions to raise arithmetic exceptions
-- (raiseDivZero, raiseUnderflow, raiseOverflow) that are wired-in the RTS.
-- These exceptions are meant to be used by the package implementing arbitrary
-- precision numbers (Natural,Integer). It can't depend on `base` package to
-- raise exceptions in a normal way because it would create a dependency
-- cycle (base <-> bignum package). See #14664
--
-- See also: Note [Wired-in exceptions are not CAFfy] in GHC.Core.Make.

-- | Raise 'GHC.Exception.Type.overflowException'
raiseOverflow :: a
raiseOverflow = raiseOverflow# (# #)

-- | Raise 'GHC.Exception.Type.underflowException'
raiseUnderflow :: a
raiseUnderflow = raiseUnderflow# (# #)

-- | Raise 'GHC.Exception.Type.divZeroException'
raiseDivZero :: a
raiseDivZero = raiseDivZero# (# #)
