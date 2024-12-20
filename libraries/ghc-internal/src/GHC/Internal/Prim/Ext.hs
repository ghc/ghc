{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}

-- We need platform defines (tests for mingw32 below).
#include "ghcplatform.h"
#include "MachDeps.h"

-- See note [When do out-of-line primops go in primops.txt.pp]. More primops
-- there are eligible according to the description below, but cannot yet be moved
-- here because of superficial restrictions to `foreign import prim`. Hopefully
-- that is fixed soon.

-- | Extra C-- routines exposed from the RTS
--
-- Users should not import this module.  It is GHC internal only.  Use
-- "GHC.Conc" instead.
--
-- Actual primops are emitted by the compiler itself. They are special bits of
-- code with backend support. The foreign functions in this module aren't actual
-- primops because the compiler doesn't care about them at all: they just are
-- extra foreign C-- calls libraries can make into the RTS.
--
-- Note that 'GHC.Internal.Prim' has the same haddock section names as this module, but
-- with descriptions. Consult that module's documentation for what each section means.
-- are described over there.
module GHC.Internal.Prim.Ext
  (
  -- * Misc
    getThreadAllocationCounter#
  -- * Delay\/wait operations
#if defined(mingw32_HOST_OS)
  , asyncRead#
  , asyncWrite#
  , asyncDoProc#
#endif
  ) where

import GHC.Internal.Prim

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()

default () -- Double and Integer aren't available yet

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
  -> (# State# RealWorld, Int64# #)

------------------------------------------------------------------------
-- Rules for primops that don't need to be built-in
------------------------------------------------------------------------

-- All these rules are used to remove useless casts:
--
--  1. passing through a type with at least the same bit size:
--    e.g. Int8# -> Int# -> Int8#
--         ==> id
--
--  2. passing through a (un)signed type of the same bit size:
--    e.g. Word# -> Int# -> Word#
--         ==> id
--
--  3. one of the previous cases with signedness change:
--    e.g. Int8# -> Int# -> Word# -> Word8#
--         ==> Int8# -> Word8#
--


-- case 1:
-- ~~~~~~~

{-# RULES

"Int8# -> Int# -> Int8#"
  forall x . intToInt8# (int8ToInt# x) = x

"Int16# -> Int# -> Int16#"
  forall x . intToInt16# (int16ToInt# x) = x

"Int32# -> Int# -> Int32#"
  forall x . intToInt32# (int32ToInt# x) = x


"Word8# -> Word# -> Word8#"
  forall x . wordToWord8# (word8ToWord# x) = x

"Word16# -> Word# -> Word16#"
  forall x . wordToWord16# (word16ToWord# x) = x

"Word32# -> Word# -> Word32#"
  forall x . wordToWord32# (word32ToWord# x) = x


"Int# -> Int64# -> Int#"
  forall x . int64ToInt# (intToInt64# x) = x

"Word# -> Word64# -> Word#"
  forall x . word64ToWord# (wordToWord64# x) = x

#-}

#if WORD_SIZE_IN_BITS == 64
{-# RULES

"Int64# -> Int# -> Int64#"
  forall x . intToInt64# (int64ToInt# x) = x

"Word64# -> Word# -> Word64#"
  forall x . wordToWord64# (word64ToWord# x) = x

#-}
#endif


-- case 2:
-- ~~~~~~~

{-# RULES

"Word# -> Int# -> Word#"
  forall x . int2Word# (word2Int# x) = x

"Int# -> Word# -> Int#"
  forall x . word2Int# (int2Word# x) = x

"Int8# -> Word8# -> Int8#"
  forall x . word8ToInt8# (int8ToWord8# x) = x

"Word8# -> Int8# -> Word8#"
  forall x . int8ToWord8# (word8ToInt8# x) = x

"Int16# -> Word16# -> Int16#"
  forall x . word16ToInt16# (int16ToWord16# x) = x

"Word16# -> Int16# -> Word16#"
  forall x . int16ToWord16# (word16ToInt16# x) = x

"Int32# -> Word32# -> Int32#"
  forall x . word32ToInt32# (int32ToWord32# x) = x

"Word32# -> Int32# -> Word32#"
  forall x . int32ToWord32# (word32ToInt32# x) = x

"Int64# -> Word64# -> Int64#"
  forall x . word64ToInt64# (int64ToWord64# x) = x

"Word64# -> Int64# -> Word64#"
  forall x . int64ToWord64# (word64ToInt64# x) = x

#-}

-- case 3:
-- ~~~~~~~

{-# RULES

"Int8# -> Int# -> Word# -> Word8#"
  forall x . wordToWord8# (int2Word# (int8ToInt# x)) = int8ToWord8# x

"Int16# -> Int# -> Word# -> Word16#"
  forall x . wordToWord16# (int2Word# (int16ToInt# x)) = int16ToWord16# x

"Int32# -> Int# -> Word# -> Word32#"
  forall x . wordToWord32# (int2Word# (int32ToInt# x)) = int32ToWord32# x

"Word8# -> Word# -> Int# -> Int8#"
  forall x . intToInt8# (word2Int# (word8ToWord# x)) = word8ToInt8# x

"Word16# -> Word# -> Int# -> Int16#"
  forall x . intToInt16# (word2Int# (word16ToWord# x)) = word16ToInt16# x

"Word32# -> Word# -> Int# -> Int32#"
  forall x . intToInt32# (word2Int# (word32ToWord# x)) = word32ToInt32# x

"Word# -> Word64# -> Int64# -> Int#"
  forall x. int64ToInt# (word64ToInt64# (wordToWord64# x)) = word2Int# x

"Int# -> Int64# -> Word64# -> Word#"
  forall x. word64ToWord# (int64ToWord64# (intToInt64# x)) = int2Word# x

#-}

#if WORD_SIZE_IN_BITS == 64
{-# RULES
"Int64# -> Int# -> Word# -> Word64#"
  forall x . wordToWord64# (int2Word# (int64ToInt# x)) = int64ToWord64# x

"Word64# -> Word# -> Int# -> Int64#"
  forall x . intToInt64# (word2Int# (word64ToWord# x)) = word64ToInt64# x
#-}
#endif


-- Push downcast into bitwise operations
{-# RULES
"word64ToWord#/and64#"
  forall x y . word64ToWord# (and64# x y) = and# (word64ToWord# x) (word64ToWord# y)

"word64ToWord#/or64#"
  forall x y . word64ToWord# (or64# x y) = or# (word64ToWord# x) (word64ToWord# y)

"word64ToWord#/xor64#"
  forall x y . word64ToWord# (xor64# x y) = xor# (word64ToWord# x) (word64ToWord# y)

#-}
