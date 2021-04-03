{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes#-}
{-# LANGUAGE GHCForeignImportPrim #-}

-- |
-- This module exposes an interface for capturing the state of a thread's
-- execution stack for diagnostics purposes: 'cloneMyStack',
-- 'cloneThreadStack'.
--
-- Such a "cloned" stack can be decoded with 'decode' to a stack trace, given
-- that the @-finfo-table-map@ is enabled.
--
-- @since 2.16.0.0
module GHC.Stack.CloneStack (
  StackSnapshot(..),
  StackEntry(..),
  cloneMyStack,
  cloneThreadStack,
  decode
  ) where

import Control.Concurrent.MVar
import Data.Maybe (catMaybes)
import Foreign
import GHC.Conc.Sync
import GHC.Exts (Int (I#), RealWorld, StackSnapshot#, ThreadId#, Array#, sizeofArray#, indexArray#, State#, StablePtr#)
import GHC.IO (IO (..))
import GHC.Stack.CCS (InfoProv (..), InfoProvEnt, ipeProv, peekInfoProv)
import GHC.Stable

-- | A frozen snapshot of the state of an execution stack.
--
-- @since 2.16.0.0
data StackSnapshot = StackSnapshot !StackSnapshot#

foreign import prim "stg_decodeStackzh" decodeStack# :: StackSnapshot# -> State# RealWorld -> (# State# RealWorld, Array# (Ptr InfoProvEnt) #)

foreign import prim "stg_cloneMyStackzh" cloneMyStack# :: State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

foreign import prim "stg_sendCloneStackMessagezh" sendCloneStackMessage# :: ThreadId# -> StablePtr# PrimMVar -> State# RealWorld -> (# State# RealWorld, (# #) #)

{-
Note [Stack Cloning]
~~~~~~~~~~~~~~~~~~~~
"Cloning" a stack means that it's `StgStack` closure is copied including the
stack memory (`stack[]`). Closures referenced by stack closures are not copied,
i.e. pointer payloads are still referred to by the same pointer.
In other words: Only those parts that are affected by stack evaluation are
"cloned".

The stack pointer (sp) of the clone is adjusted to be valid, i.e. to point into
the cloned stack.

The clone is "offline"/"cold", i.e. it won't be evaluated any further. This is
useful for further analyses like stack unwinding or traversal because all
pointers stay valid.

StackSnapshot#
--------------
A cloned stack is represented in Haskell by `StackSnapshot !StackSnapshot#`.
`StackSnapshot#` is a primitive type, it's value is a pointer to the stack in
RTS (`StgStack*`).

To take advantage of the garbage collector, the representation cannot be `Ptr`
or `StablePtr`:
- Closures referenced by a `Ptr` may be garbage collected at any time (without
  checking if it's still in use).
- `StablePtr` has to be freed explictly, which would introduce nasty state
   handling.

By using a primitive type, the stack closure (and its transitive closures) is
kept and managed by the garbage collector as long as it's in use and
automatically freed later.
As closures referred to by stack closures (e.g. payloads) may be used by other
closures that are not related to stack cloning, the memory has to be managed by
the garbage collector; i.e. one cannot simply call free() in the RTS C code
because it's hard to figure out what to free while the garbage collector is
built to do this job.

RTS interface
-------------
There are two different ways to clone a stack:
1. `cloneMyStack#` - A primop for cloning the active thread's stack.
2. `sendCloneStackMessage#` - A primop for cloning another thread's stack.
   Sends a RTS message (Messages.c) with a MVar to that thread. The cloned
   stack is reveived by taking it out of this MVar.

`cloneMyStack#` has to be a primop, because new primitive types
(`StackSnapshot#`) cannot be marshalled by FFI. Using a `Ptr StackSnapshot` as
FFI return type would not save the snapshot from being garbage collected, as
discussed in the section above.

C API
-------------
`cloneStack` is the function that really clones a given stack and returns
the clone:
`StgStack* cloneStack(Capability* capability, const StgStack* stack)`

It's called directly by `stg_cloneMyStackzh` (`PrimOps.cmm`), the
`cloneMyStack#` primop.

To clone another thread's stack, there's a message passing mechanism such that
the receiver's capability clones its. So, there's no need to stop/pause the
other thread as it's capability will fulfill the cloning request when it's
ready to do so.

The message is defined in `Closures.h`:

```
typedef struct MessageCloneStack_ {
    StgHeader header;
    Message   *link;
    StgMVar   *result;
    StgTSO    *tso;
} MessageCloneStack;
```

The fields are:
- `header`: It's a closure and thus subject to garbage collection (no manual
   memory management needed)
- `link`: Messages form a singly linked list in `Capability`, referred to by
  `capability->inbox`.
- `result`: An `MVar`. When the message is sent it's empty, after cloning the
  `StackSnapshot` is put into it.
- `tso`: `tso->stackobj` is the stack to clone.

The asynchronous flow can be split into sending this message and putting the
cloned stack into the MVar (expecting the sender to get it from there).

Sending:
The public C function to send is
`void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar)`.
It prepares the message for the thread to clone (identified by it's `tso`) and
sets the `result` MVar (pointed to by `mvar`). Then it sends the message by
calling `sendMessage` which puts it into the Capabilities `inbox`.

Receiving:
Inbox processing is part of the big work finding loop in `schedule`. The
function that dispatches messages is `executeMessage`. From there
`void handleCloneStackMessage(MessageCloneStack *msg)` is called.

`handleCloneStackMessage` clones the stack, lifts the result to `StackSnapshot`
(MVar needs a lifted value, no primitive) and puts it into the MVar
(`msg->mvar`).
-}

{-
Note [Stack Decoding]
~~~~~~~~~~~~~~~~~~~~~
A cloned stack is decoded (unwound) by looking up the Info Table Provenance
Entries (IPE) for every stack frame with `lookupIPE` in the RTS.

The IPEs contain source locations and are pulled from the RTS/C world into
Haskell.

RTS interface
-------------

The primop decodeStack# returns an array of IPE pointers that are later
unmarshalled with HSC. If there is no IPE for a return frame (which can easily
happen when a library wasn't compiled with `-finfo-table-map`), it's
represented by a null pointer.

Caveats:
- decodeStack# has to be a primop (not a simple C FFI function), because
  there always has to be at least one active `TSO`. Otherwise, allocating
  memory with the garbage collector for the returned value fails.
- decodeStack# has to be defined outside of `primops.txt.pp` because its
  return type `Array# (Ptr InfoProvEnt)` cannot be defined there:
  `InfoProvEnt` and `Ptr` would have to be imported which seems to be too
  specific for this file.

Notes
-----
The relevant notes are:
  - Note [Mapping Info Tables to Source Positions]
  - Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
-}

-- | Clone the stack of the executing thread
--
-- @since 2.16.0.0
cloneMyStack :: IO StackSnapshot
cloneMyStack = IO $ \s ->
   case (cloneMyStack# s) of (# s1, stack #) -> (# s1, StackSnapshot stack #)

-- | Clone the stack of a thread identified by its 'ThreadId'
--
-- @since 2.16.0.0
cloneThreadStack :: ThreadId -> IO StackSnapshot
cloneThreadStack (ThreadId tid#) = do
  resultVar <- newEmptyMVar @StackSnapshot
  boxedPtr@(StablePtr ptr) <- newStablePtrPrimMVar resultVar
  -- Use the RTS's "message" mechanism to request that
  -- the thread captures its stack, saving the result
  -- into resultVar.
  IO $ \s -> case sendCloneStackMessage# tid# ptr s of (# s', (# #) #) -> (# s', () #)
  freeStablePtr boxedPtr
  takeMVar resultVar

-- | Represetation for the source location where a return frame was pushed on the stack.
-- This happens every time when a @case ... of@ scrutinee is evaluated.
data StackEntry = StackEntry
  { functionName :: String,
    moduleName :: String,
    srcLoc :: String,
    closureType :: Word
  }
  deriving (Show, Eq)

-- | Decode a 'StackSnapshot' to a stacktrace (a list of 'StackEntry').
-- The stack trace is created from return frames with according 'InfoProvEnt'
-- entries. To generate them, use the GHC flag @-finfo-table-map@. If there are
-- no 'InfoProvEnt' entries, an empty list is returned.
--
-- Please note:
--
--   * To gather 'StackEntry' from libraries, these have to be
--     compiled with @-finfo-table-map@, too.
--   * Due to optimizations by GHC (e.g. inlining) the stacktrace may change
--     with different GHC parameters and versions.
--   * The stack trace is empty (by design) if there are no return frames on
--     the stack. (These are pushed every time when a @case ... of@ scrutinee
--     is evaluated.)
--
-- @since 2.16.0.0
decode :: StackSnapshot -> IO [StackEntry]
decode stackSnapshot = do
    stackEntries <- getDecodedStackArray stackSnapshot
    ipes <- mapM unmarshall stackEntries
    return $ catMaybes ipes

    where
      unmarshall :: Ptr InfoProvEnt -> IO (Maybe StackEntry)
      unmarshall ipe = if ipe == nullPtr then
                          pure Nothing
                       else do
                          infoProv <- (peekInfoProv . ipeProv) ipe
                          pure $ Just (toStackEntry infoProv)
      toStackEntry :: InfoProv -> StackEntry
      toStackEntry infoProv =
        StackEntry
        { functionName = ipLabel infoProv,
          moduleName = ipMod infoProv,
          srcLoc = ipLoc infoProv,
          -- read looks dangerous, be we can trust that the closure type is always there.
          closureType = read . ipDesc $ infoProv
        }

getDecodedStackArray :: StackSnapshot -> IO [Ptr InfoProvEnt]
getDecodedStackArray (StackSnapshot s) =
  IO $ \s0 -> case decodeStack# s s0 of
    (# s1, a #) -> (# s1, (go a ((I# (sizeofArray# a)) - 1)) #)
  where
    go :: Array# (Ptr InfoProvEnt) -> Int -> [Ptr InfoProvEnt]
    go stack 0 = [stackEntryAt stack 0]
    go stack i = (stackEntryAt stack i) : go stack (i - 1)

    stackEntryAt :: Array# (Ptr InfoProvEnt) -> Int -> Ptr InfoProvEnt
    stackEntryAt stack (I# i) = case indexArray# stack i of
      (# se #) -> se
