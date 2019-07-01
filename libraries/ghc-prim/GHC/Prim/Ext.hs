{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"

module GHC.Prim.Ext
  (
  -- * Delay\/wait operations
#if defined(mingw32_TARGET_OS)
  , asyncRead#
  , asyncWrite#
  , asyncDoProc#
#endif

  ) where

#if defined(mingw32_TARGET_OS)
import GHC.Prim
#endif

default () -- Double and Integer aren't available yet

------------------------------------------------------------------------
-- Delay/wait operations
------------------------------------------------------------------------

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
