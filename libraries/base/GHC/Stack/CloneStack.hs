{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes#-}

-- |
-- This module exposes an interface for capturing the state of a thread's
-- execution stack for diagnostics purposes.
--
-- @since 2.16.0.0
module GHC.Stack.CloneStack (
  StackSnapshot(..),
  cloneMyStack,
  cloneThreadStack
  ) where

import GHC.Prim (StackSnapshot#, cloneMyStack#, ThreadId#)
import Control.Concurrent.MVar
import GHC.Conc.Sync
import GHC.Stable
import GHC.IO (IO(..))

-- | A frozen snapshot of the state of an execution stack.
--
-- @since 2.16.0.0
data StackSnapshot = StackSnapshot !StackSnapshot#

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

By using a primitive type, the stack closure is kept and managed by the garbage
collector as long as it's in use and automatically freed later.
As closures referred to by stack closures (e.g. payloads) may be used by other
closures that are not related to stack cloning, the memory has to be managed by
the garbage collector; i.e. one cannot simply call free() in the RTS C code
because it's hard to figure out what to free while the garbage collector is
built to do this job.

RTS interface
-------------
There are two different ways to clone a stack:
1. `cloneMyStack#` - A primop for cloning the active thread's stack.
2. `sendCloneStackMessage` - A FFI function for cloning another thread's stack.
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

-- | Clone the stack of the executing thread
--
-- @since 2.16.0.0
cloneMyStack :: IO StackSnapshot
cloneMyStack = IO $ \s ->
   case (cloneMyStack# s) of (# s1, stack #) -> (# s1, StackSnapshot stack #)

foreign import ccall "sendCloneStackMessage" sendCloneStackMessage :: ThreadId# -> StablePtr PrimMVar -> IO ()

-- | Clone the stack of a thread identified by its 'ThreadId'
--
-- @since 2.16.0.0
cloneThreadStack :: ThreadId -> IO StackSnapshot
cloneThreadStack (ThreadId tid#) = do
  resultVar <- newEmptyMVar @StackSnapshot
  ptr <- newStablePtrPrimMVar resultVar
  -- Use the RTS's "message" mechanism to request that
  -- the thread captures its stack, saving the result
  -- into resultVar.
  sendCloneStackMessage tid# ptr
  freeStablePtr ptr
  takeMVar resultVar
