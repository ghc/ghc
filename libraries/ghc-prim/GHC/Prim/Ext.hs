{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"

-- {-# LANGUAGE PrimImport#-}
module GHC.Prim.Ext
  (
  -- * Double#
  -- |Operations on double-precision (64 bit) floating-point numbers.
    decodeDouble_2Int#
  , decodeDouble_Int64
  -- * Float#
  -- |Operations on single-precision (32-bit) floating-point numbers.
  , decodeFloat_Int#
  -- * Delay\/wait operations
  -- |
#if defined(mingw32_TARGET_OS)
  , asyncRead#
  , asyncWrite#
  , asyncDoProc#
#endif

  ) where

import GHC.Prim

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif


-- * Double#
-- |Operations on double-precision (64 bit) floating-point numbers.

-- | Convert to integer.
-- First component of the result is -1 or 1, indicating the sign of the
-- mantissa. The next two are the high and low 32 bits of the mantissa
-- respectively, and the last is the exponent.
foreign import prim "decodeDouble_2Int" decodeDouble_2Int#
  :: Double#
  -> (# Int#, Word#, Word#, Int# #)

-- | Decode 'Double' into mantissa and base-2 exponent.
foreign import prim  "decodeDouble_Int64" decodeDouble_Int64
  :: Double#
  -> (# INT64, Int# #)


-- | Convert to integers.
-- First {\tt Int\#} in result is the mantissa; second is the exponent.}
foreign import prim "decodeFloat_Int" decodeFloat_Int#
  :: Float# -> (# Int#, Int# #)

-- * Delay\/wait operations
-- |

#if defined(mingw32_TARGET_OS)

-- | Asynchronously read bytes from specified file descriptor.
foreign import prim "asyncRead" asyncRead#
  ::  Int#
  -> Int#
  -> Int#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

-- | Asynchronously write bytes from specified file descriptor.
foreign import prim "asyncWrite" asyncWrite#
  :: Int#
  -> Int#
  -> Int#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

-- | Asynchronously perform procedure (first arg), passing it 2nd arg.
foreign import prim "asyncDoProc" asyncDoProc#
  :: Addr#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

#endif
