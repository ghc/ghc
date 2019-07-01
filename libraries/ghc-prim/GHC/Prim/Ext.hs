{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"
#include "MachDeps.h"

-- | For section descriptions, see 'GHC.Prim'.
module GHC.Prim.Ext
  (
  -- 64-bit bit aliases
    INT64
  , WORD64
  -- * Delay\/wait operations
#if defined(mingw32_TARGET_OS)
  , asyncRead#
  , asyncWrite#
  , asyncDoProc#
#endif
  -- * Misc
  , getThreadAllocationCounter#
  ) where

import GHC.Prim
import GHC.Types () -- Make implicit dependency known to build system

default () -- Double and Integer aren't available yet

------------------------------------------------------------------------
-- 64-bit bit aliases
------------------------------------------------------------------------

type INT64 =
#if WORD_SIZE_IN_BITS < 64
  Int64#
#else
  Int#
#endif

type WORD64 =
#if WORD_SIZE_IN_BITS < 64
  Word64#
#else
  Word#
#endif

------------------------------------------------------------------------
-- Delay/wait operations
------------------------------------------------------------------------

#if defined(mingw32_TARGET_OS)

-- | Asynchronously read bytes from specified file descriptor.
foreign import prim "asyncRead" asyncRead#
  :: Int#
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

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

-- | Retrieves the allocation counter for the current thread.
foreign import prim "getThreadAllocationCounter" getThreadAllocationCounter#
  :: State# RealWorld
  -> (# State# RealWorld, INT64 #)
