{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}

-- | Primitive exceptions.
module GHC.Prim.Exception
   ( raiseOverflow
   , raiseUnderflow
   , raiseDivZero
   )
where

import GHC.Prim
import GHC.Magic

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

foreign import prim "stg_raiseOverflowzh" raiseOverflow# :: State# RealWorld -> (# State# RealWorld, (# #) #)
foreign import prim "stg_raiseUnderflowzh" raiseUnderflow# :: State# RealWorld -> (# State# RealWorld, (# #) #)
foreign import prim "stg_raiseDivZZerozh" raiseDivZero# :: State# RealWorld -> (# State# RealWorld, (# #) #)

-- We give a bottoming demand signature to 'raiseOverflow', 'raiseUnderflow' and
-- 'raiseDivZero' in "GHC.Core.Make". NOINLINE pragmas are necessary because if
-- we ever inlined them we would lose that information.

-- | Raise 'GHC.Exception.Type.overflowException'
raiseOverflow :: a
{-# NOINLINE raiseOverflow #-}
raiseOverflow = runRW# (\s -> case raiseOverflow# s of (# _, _ #) -> let x = x in x)

-- | Raise 'GHC.Exception.Type.underflowException'
raiseUnderflow :: a
{-# NOINLINE raiseUnderflow #-}
raiseUnderflow = runRW# (\s -> case raiseUnderflow# s of (# _, _ #) -> let x = x in x)

-- | Raise 'GHC.Exception.Type.divZeroException'
raiseDivZero :: a
{-# NOINLINE raiseDivZero #-}
raiseDivZero = runRW# (\s -> case raiseDivZero# s of (# _, _ #) -> let x = x in x)
