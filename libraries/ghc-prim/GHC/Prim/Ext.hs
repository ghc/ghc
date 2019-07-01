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
  -- * Arrays
  , unsafeThawArray#
  , casArray#
  -- * Small Arrays
  , unsafeThawSmallArray#
  , casSmallArray#
  -- * Byte Arrays
  , newPinnedByteArray#
  , newAlignedPinnedByteArray#
  , shrinkMutableByteArray#
  , resizeMutableByteArray#
  -- * Arrays of Arrays
  , newArrayArray#
  -- * Mutable variables
  , newMutVar#
  , casMutVar#
  -- * STM-accessible Mutable Variables
  , newTVar#
  , readTVar#
  , readTVarIO#
  , writeTVar#
  -- * Synchronized Mutable Variables
  , newMVar#
  , takeMVar#
  , tryTakeMVar#
  , putMVar#
  , tryPutMVar#
  , readMVar#
  , tryReadMVar#
  , isEmptyMVar#
  -- * Delay\/wait operations
  , delay#
  , waitRead#
  , waitWrite#
#if defined(mingw32_HOST_OS)
  , asyncRead#
  , asyncWrite#
  , asyncDoProc#
#endif
  -- * Concurrency primitives
  , fork#
  , forkOn#
  , killThread#
  , yield#
  , labelThread#
  , isCurrentThreadBound#
  , noDuplicate#
  , threadStatus#
  -- * Weak pointers
  , mkWeak#
  , mkWeakNoFinalizer#
  , addCFinalizerToWeak#
  , deRefWeak#
  , finalizeWeak#
  -- * Stable pointers and names
  , makeStablePtr#
  , deRefStablePtr#
  , makeStableName#
  -- * Compact normal form
  , compactNew#
  , compactResize#
  , compactContains#
  , compactContainsAny#
  , compactGetFirstBlock#
  , compactGetNextBlock#
  , compactAllocateBlock#
  , compactFixupPointers#
  , compactAdd#
  , compactAddWithSharing#
  , compactSize#
  -- * Parallelism
  , getSpark#
  , numSparks#
  -- * Bytecode operations
  , mkApUpd0#
  , newBCO#
  , unpackClosure#
  , closureSize#
  , getApStackVal#
  -- * Misc
  , clearCCS#
  , traceEvent#
  , traceBinaryEvent#
  , traceMarker#
  , getThreadAllocationCounter#
  , setThreadAllocationCounter#
  ) where

import GHC.Prim
import GHC.Types () -- Make implicit dependency known to build system

default () -- Double and Integer aren't available yet

------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------

-- | Make an immutable array mutable, without copying.
foreign import prim "stg_unsafeThawArrayzh" unsafeThawArray#
  :: Array# a
  -> State# s
  -> (# State# s, MutableArray# s a #)

-- | Given an array, an offset, the expected old value, and
-- the new value, perform an atomic compare and swap (i.e. write the new
-- value if the current value and the old value are the same pointer).
-- Returns 0 if the swap succeeds and 1 if it fails. Additionally, returns
-- the element at the offset after the operation completes. This means that
-- on a success the new value is returned, and on a failure the actual old
-- value (not the expected one) is returned. Implies a full memory barrier.
-- The use of a pointer equality on a lifted value makes this function harder
-- to use correctly than 'casIntArray#'. All of the difficulties
-- of using 'reallyUnsafePtrEquality#' correctly apply to
-- 'casArray#' as well.
foreign import prim "stg_casArrayzh" casArray#
  :: MutableArray# s a
  -> Int#
  -> a
  -> a
  -> State# s
  -> (# State# s, Int#, a #)

------------------------------------------------------------------------
-- Small Arrays
------------------------------------------------------------------------

-- | Make an immutable array mutable, without copying.
foreign import prim "stg_unsafeThawSmallArrayzh" unsafeThawSmallArray#
  :: SmallArray# a -> State# s -> (# State# s, SmallMutableArray# s a #)

-- | Unsafe, machine-level atomic compare and swap on an element within an array.
-- See the documentation of 'casArray#'.
foreign import prim "stg_casSmallArrayzh" casSmallArray#
  :: SmallMutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)

------------------------------------------------------------------------
-- Byte Arrays
------------------------------------------------------------------------

-- | Create a mutable byte array that the GC guarantees not to move.
foreign import prim "stg_newPinnedByteArrayzh" newPinnedByteArray#
  :: Int# -> State# s -> (# State# s, MutableByteArray# s #)

-- | Create a mutable byte array, aligned by the specified amount, that the GC
-- guarantees not to move.
foreign import prim "stg_newAlignedPinnedByteArrayzh" newAlignedPinnedByteArray#
  :: Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)

-- | Shrink mutable byte array to new specified size (in bytes), in
-- the specified state thread. The new size argument must be less than or
-- equal to the current size as reported by 'sizeofMutableArray#'.
foreign import prim "stg_shrinkMutableByteArrayzh" shrinkMutableByteArray#
  :: MutableByteArray# s -> Int# -> State# s -> State# s

-- | Resize (unpinned) mutable byte array to new specified size (in bytes).
-- The returned 'MutableByteArray#' is either the original
-- 'MutableByteArray#' resized in-place or, if not possible, a newly
-- allocated (unpinned) 'MutableByteArray#' (with the original content
-- copied over).
--
-- To avoid undefined behaviour, the original 'MutableByteArray#' shall
-- not be accessed anymore after a 'resizeMutableByteArray#' has been
-- performed.  Moreover, no reference to the old one should be kept in order
-- to allow garbage collection of the original 'MutableByteArray#' in
-- case a new 'MutableByteArray#' had to be allocated.
foreign import prim "stg_resizeMutableByteArrayzh" resizeMutableByteArray#
  :: MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)

------------------------------------------------------------------------
-- Arrays of Arrays
------------------------------------------------------------------------

-- | Create a new mutable array of arrays with the specified number of elements,
-- in the specified state thread, with each element recursively referring to the
-- newly created array.
foreign import prim "stg_newArrayArrayzh" newArrayArray#
  :: Int#
  -> State# s
  -> (# State# s, MutableArrayArray# s #)

------------------------------------------------------------------------
-- Mutable variables
------------------------------------------------------------------------

-- | Create 'MutVar#' with specified initial value in specified state thread.
foreign import prim "stg_newMutVarzh" newMutVar#
  :: a -> State# s -> (# State# s, MutVar# s a #)

foreign import prim "stg_casMutVarzh" casMutVar#
  :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)

------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

foreign import prim "stg_getMaskingStatezh" getMaskingState#
  :: State# RealWorld -> (# State# RealWorld, Int# #)

------------------------------------------------------------------------
-- STM-accessible Mutable Variables
------------------------------------------------------------------------

-- | Create a new 'TVar#' holding a specified initial value.
foreign import prim "stg_newTVarzh" newTVar#
  :: a
  -> State# s
  -> (# State# s, TVar# s a #)

-- | Read contents of 'TVar#'.  Result is not yet evaluated.
foreign import prim "stg_readTVarzh" readTVar#
  :: TVar# s a
  -> State# s
  -> (# State# s, a #)

-- | Read contents of 'TVar#' outside an STM transaction.
foreign import prim "stg_readTVarIOzh" readTVarIO#
  :: TVar# s a
  -> State# s -> (# State# s, a #)

-- | Write contents of 'TVar#'.
foreign import prim "stg_writeTVarzh" writeTVar#
  :: TVar# s a
  -> a
  -> State# s
  -> State# s

------------------------------------------------------------------------
-- Synchronized Mutable Variables
------------------------------------------------------------------------

-- | Create new 'MVar#'; initially empty.
foreign import prim "stg_newMVarzh" newMVar#
  :: State# s -> (# State# s, MVar# s a #)

-- | If 'MVar#' is empty, block until it becomes full.
-- Then remove and return its contents, and set it empty.
foreign import prim "stg_takeMVarzh" takeMVar#
  :: MVar# s a -> State# s -> (# State# s, a #)

-- | If 'MVar#' is empty, immediately return with integer 0 and value undefined.
-- Otherwise, return with integer 1 and contents of 'MVar#', and set 'MVar#' empty.
foreign import prim "stg_tryTakeMVarzh" tryTakeMVar#
  :: MVar# s a -> State# s -> (# State# s, Int#, a #)

-- | If 'MVar#' is full, block until it becomes empty.
foreign import prim "stg_putMVarzh" putMVar#
  :: MVar# s a -> a -> State# s -> State# s

-- | If 'MVar#' is full, immediately return with integer 0.
-- | Otherwise, store value arg as 'MVar#''s new contents, and return with integer 1.
foreign import prim "stg_tryPutMVarzh" tryPutMVar#
  :: MVar# s a -> a -> State# s -> (# State# s, Int# #)

-- | If 'MVar#' is empty, block until it becomes full.
-- Then read its contents without modifying the MVar, without possibility
-- of intervention from other threads.
foreign import prim "stg_readMVarzh" readMVar#
  :: MVar# s a -> State# s -> (# State# s, a #)

-- | If 'MVar#' is empty, immediately return with integer 0 and value undefined.
-- Otherwise, return with integer 1 and contents of 'MVar#'.
foreign import prim "stg_tryReadMVarzh" tryReadMVar#
  :: MVar# s a -> State# s -> (# State# s, Int#, a #)

-- | Return 1 if 'MVar#' is empty; 0 otherwise.
foreign import prim "stg_isEmptyMVarzh" isEmptyMVar#
  :: MVar# s a -> State# s -> (# State# s, Int# #)

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

-- | Sleep specified number of microseconds.
foreign import prim "stg_delayzh" delay#
  :: Int# -> State# s -> State# s

-- | Block until input is available on specified file descriptor.
foreign import prim "stg_waitReadzh" waitRead#
  :: Int# -> State# s -> State# s

-- | Block until output is possible on specified file descriptor.
foreign import prim "stg_waitWritezh" waitWrite#
  :: Int# -> State# s -> State# s

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
-- Concurrency primitives
------------------------------------------------------------------------

foreign import prim "stg_forkzh" fork#
  :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)

foreign import prim "stg_forkOnzh" forkOn#
  :: Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)

foreign import prim "stg_killThreadzh" killThread#
  :: ThreadId# -> a -> State# RealWorld -> State# RealWorld

foreign import prim "stg_yieldzh" yield#
  :: State# RealWorld -> State# RealWorld

foreign import prim "stg_labelThreadzh" labelThread#
  :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld

foreign import prim "stg_isCurrentThreadBoundzh" isCurrentThreadBound#
  :: State# RealWorld -> (# State# RealWorld, Int# #)

foreign import prim "stg_noDuplicatezh" noDuplicate#
  :: State# s -> State# s

foreign import prim "stg_threadStatuszh" threadStatus#
  :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)

------------------------------------------------------------------------
-- Weak pointers
------------------------------------------------------------------------

-- | 'mkWeak# k v finalizer s' creates a weak reference to value 'k',
-- with an associated reference to some value 'v'. If 'k' is still
-- alive then 'v' can be retrieved using 'deRefWeak#'. Note that
-- the type of 'k' must be represented by a pointer (i.e. of kind
-- 'TYPE LiftedRep' or 'TYPE UnliftedRep').
foreign import prim "stg_mkWeakzh" mkWeak#
  :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #))
     -> State# RealWorld -> (# State# RealWorld, Weak# b #)

foreign import prim "stg_mkWeakNoFinalizerzh" mkWeakNoFinalizer#
  :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)

-- | 'addCFinalizerToWeak# fptr ptr flag eptr w' attaches a C
-- function pointer 'fptr' to a weak pointer 'w' as a finalizer. If
-- 'flag' is zero, 'fptr' will be called with one argument,
-- 'ptr'. Otherwise, it will be called with two arguments,
-- 'eptr' and 'ptr'. 'addCFinalizerToWeak#' returns
-- 1 on success, or 0 if 'w' is already dead.
foreign import prim "stg_addCFinalizerToWeakzh" addCFinalizerToWeak#
  :: Addr# -> Addr# -> Int# -> Addr# -> Weak# b
          -> State# RealWorld -> (# State# RealWorld, Int# #)

foreign import prim "stg_deRefWeakzh" deRefWeak#
  :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)

-- | Finalize a weak pointer. The return value is an unboxed tuple
-- containing the new state of the world and an "stg_unboxed Maybe",
-- represented by an 'Int#' and a (possibly invalid) finalization
-- action. An 'Int#' of '1' indicates that the finalizer is valid. The
-- return value 'b' from the finalizer should be ignored.
foreign import prim "stg_finalizeWeakzh" finalizeWeak#
  :: Weak# a
  -> State# RealWorld
  -> (# State# RealWorld
     ,  Int#
     , (State# RealWorld -> (# State# RealWorld, b #) )
     #)

------------------------------------------------------------------------
-- Stable pointers and names
------------------------------------------------------------------------

foreign import prim "stg_makeStablePtrzh" makeStablePtr#
  :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)

foreign import prim "stg_deRefStablePtrzh" deRefStablePtr#
  :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)

foreign import prim "stg_makeStableNamezh" makeStableName#
  :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)

------------------------------------------------------------------------
-- Compact normal form
------------------------------------------------------------------------

-- | Create a new CNF with a single compact block. The argument is
-- the capacity of the compact block (in bytes, not words).
-- The capacity is rounded up to a multiple of the allocator block size
-- and is capped to one mega block.
foreign import prim "stg_compactNewzh" compactNew#
  :: Word# -> State# RealWorld -> (# State# RealWorld, Compact# #)

-- | Set the new allocation size of the CNF. This value (in bytes)
-- determines the capacity of each compact block in the CNF. It
-- does not retroactively affect existing compact blocks in the CNF.
foreign import prim "stg_compactResizezh" compactResize#
  :: Compact# -> Word# -> State# RealWorld ->
   State# RealWorld

-- | Returns 1\# if the object is contained in the CNF, 0\# otherwise.
foreign import prim "stg_compactContainszh" compactContains#
  :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, Int# #)

-- | Returns 1\# if the object is in any CNF at all, 0\# otherwise.
foreign import prim "stg_compactContainsAnyzh" compactContainsAny#
  :: a -> State# RealWorld -> (# State# RealWorld, Int# #)

-- | Returns the address and the utilized size (in bytes) of the
-- first compact block of a CNF.
foreign import prim "stg_compactGetFirstBlockzh" compactGetFirstBlock#
  :: Compact# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)

-- | Given a CNF and the address of one its compact blocks, returns the
-- next compact block and its utilized size, or 'nullAddr#' if the
-- argument was the last compact block in the CNF.
foreign import prim "stg_compactGetNextBlockzh" compactGetNextBlock#
  :: Compact# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)

-- | Attempt to allocate a compact block with the capacity (in bytes) given by
-- the first argument. The 'Addr#' is a pointer to previous compact block of
-- the CNF or 'nullAddr#' to create a new CNF with a single compact block.
--
-- The resulting block is not known to the GC until 'compactFixupPointers#' is
-- called on it, and care must be taken so that the address does not escape or
-- memory will be leaked.
foreign import prim "stg_compactAllocateBlockzh" compactAllocateBlock#
  :: Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)

-- | Given the pointer to the first block of a CNF and the
-- address of the root object in the old address space, fix up
-- the internal pointers inside the CNF to account for
-- a different position in memory than when it was serialized.
-- This method must be called exactly once after importing
-- a serialized CNF. It returns the new CNF and the new adjusted
-- root address.
foreign import prim "stg_compactFixupPointerszh" compactFixupPointers#
  :: Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Compact#, Addr# #)

-- | Recursively add a closure and its transitive closure to a
-- 'Compact#' (a CNF), evaluating any unevaluated components
-- at the same time. Note: ''compactAdd#'' is not thread-safe, so
-- only one thread may call 'compactAdd\#' with a particular
-- 'Compact\#' at any given time. The primop does not
-- enforce any mutual exclusion; the caller is expected to
-- arrange this.
foreign import prim "stg_compactAddzh" compactAdd#
  :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)

-- | Like 'compactAdd\#', but retains sharing and cycles
-- during compaction.
foreign import prim "stg_compactAddWithSharingzh" compactAddWithSharing#
  :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)

-- | Return the total capacity (in bytes) of all the compact blocks
-- in the CNF.
foreign import prim "stg_compactSizezh" compactSize#
  :: Compact# -> State# RealWorld -> (# State# RealWorld, Word# #)

------------------------------------------------------------------------
-- Parallelism
------------------------------------------------------------------------

foreign import prim "stg_getSparkzh" getSpark#
  :: State# s -> (# State# s, Int#, a #)

-- | Returns the number of sparks in the local spark pool.
foreign import prim "stg_numSparkszh" numSparks#
  :: State# s -> (# State# s, Int# #)

------------------------------------------------------------------------
-- Bytecode operations
------------------------------------------------------------------------

-- | Wrap a BCO in a 'AP_UPD' thunk which will be updated with the value of
-- the BCO when evaluated.
foreign import prim "stg_mkApUpd0zh" mkApUpd0#
  :: BCO# -> (# a #)

-- | 'newBCO\# instrs lits ptrs arity bitmap' creates a new bytecode object. The
-- resulting object encodes a function of the given arity with the instructions
-- encoded in 'instrs', and a static reference table usage bitmap given by
-- 'bitmap'.
foreign import prim "stg_newBCOzh" newBCO#
  :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s, BCO# #)

-- | 'unpackClosure\# closure' copies the closure and pointers in the
--  payload of the given closure into two new arrays, and returns a pointer to
--  the first word of the closure's info table, a non-pointer array for the raw
--  bytes of the closure, and a pointer array for the pointers in the payload.
foreign import prim "stg_unpackClosurezh" unpackClosure#
  :: a -> (# Addr#, ByteArray#, Array# b #)

-- | 'closureSize\# closure' returns the size of the given closure in
-- machine words.
foreign import prim "stg_closureSizezh" closureSize#
  :: a -> Int#

foreign import prim "stg_getApStackValzh" getApStackVal#
  :: a -> Int# -> (# Int#, b #)

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

-- | Run the supplied IO action with an empty CCS.  For example, this
-- is used by the interpreter to run an interpreted computation
-- without the call stack showing that it was invoked from GHC.
foreign import prim "stg_clearCCSzh" clearCCS#
  :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)

-- | Emits an event via the RTS tracing framework.  The contents
-- of the event is the zero-terminated byte string passed as the first
-- argument.  The event will be emitted either to the '.eventlog' file,
-- or to stderr, depending on the runtime RTS flags.
foreign import prim "stg_traceEventzh" traceEvent#
  :: Addr# -> State# s -> State# s

-- | Emits an event via the RTS tracing framework.  The contents
-- of the event is the binary object passed as the first argument with
-- the the given length passed as the second argument. The event will be
-- emitted to the '.eventlog' file.
foreign import prim "stg_traceBinaryEventzh" traceBinaryEvent#
  :: Addr# -> Int# -> State# s -> State# s

-- | Emits a marker event via the RTS tracing framework.  The contents
-- of the event is the zero-terminated byte string passed as the first
-- argument.  The event will be emitted either to the '.eventlog' file,
-- or to stderr, depending on the runtime RTS flags.
foreign import prim "stg_traceMarkerzh" traceMarker#
  :: Addr# -> State# s -> State# s

-- | Retrieves the allocation counter for the current thread.
foreign import prim "stg_getThreadAllocationCounterzh" getThreadAllocationCounter#
  :: State# RealWorld
  -> (# State# RealWorld, INT64 #)

-- | Sets the allocation counter for the current thread to the given value.
foreign import prim "stg_setThreadAllocationCounterzh" setThreadAllocationCounter#
  :: INT64
  -> State# RealWorld
  -> State# RealWorld
