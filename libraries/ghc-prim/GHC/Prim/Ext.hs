{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"
#include "MachDeps.h"

-- See note [When do out-of-line primops go in primops.txt.pp]. More primops
-- there are elgible according to the description below, but cannot yet be moved
-- here because of superficial restrictions to `foreign import prim`. Hopefully
-- that is fixed soon.

-- | Extra C-- routines exposed from the RTS
--
-- Actual primops are emitted by the compiler itself. They are special bits of
-- code with backend support. The foreign functions in this module aren't actual
-- primops because the compiler doesn't care about them at all: they just are
-- extra foreign C-- calls libraries can make into the RTS.
--
-- Note that 'GHC.Prim' has the same haddock section names as this module, but
-- with descriptions. Consult that module's documentation for what each section means.
-- are described over there.
module GHC.Prim.Ext
  (
  -- 64-bit bit aliases
    INT64
  , WORD64
  -- * Delay\/wait operations
#if defined(mingw32_HOST_OS)
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

#if defined(mingw32_HOST_OS)

-- | Asynchronously read bytes from specified file descriptor.
foreign import prim "stg_asyncReadzh" asyncRead#
  :: Int#
  -> Int#
  -> Int#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

-- | Asynchronously write bytes from specified file descriptor.
foreign import prim "stg_asyncWritezh" asyncWrite#
  :: Int#
  -> Int#
  -> Int#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

-- | Asynchronously perform procedure (first arg), passing it 2nd arg.
foreign import prim "stg_asyncDoProczh" asyncDoProc#
  :: Addr#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, Int# #)

#endif

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

-- | Retrieves the allocation counter for the current thread.
foreign import prim "stg_getThreadAllocationCounterzh" getThreadAllocationCounter#
  :: State# RealWorld
  -> (# State# RealWorld, INT64 #)
