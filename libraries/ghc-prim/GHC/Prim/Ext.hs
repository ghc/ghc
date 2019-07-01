{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"
#include "MachDeps.h"

-- | External primops, extra C-- routines exposed from the RTS
--
-- Conventional primops are always, or somtimes, emitted by the compiler
-- itself. They are special bits of code with backend support.  These aren't
-- conventional primops, because the compiler doesn't care about them at all,
-- they just are extra foreign C-- calls libraries can make into the RTS.
--
-- Currently, they are all `has_side_effects = True` and `out_of_line = True`
-- (the unchangable defaults for `foreign import prim`). Not all such primops
-- are elgible to live here, however. The compiler also sometimes emits custom
-- code for a primop under certain conditions, leaving the external version as
-- a fallback. Those sometimes-external sometimes internal-primops still have
-- compiler support, so they cannot live here.
--
-- That said, there are more elgible primops to move here than are here today.
-- But those primops require extensions to `foreign import prim`.
-- Polymorphism, strictness information, and `has_side_effects = False` are
-- currently unsupported by `foreign import prim`. In the interest of removing
-- spurrious interactions between the compiler and libraries, hopefully those
-- restrictions are lifted so the compiler-aware set of primops can shrink to a
-- minimum.
--
-- Note that 'GHC.Prim' has the same section names as this module. They are
-- described over there.
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
