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
  cloneThreadStack,
  decode
  ) where

import GHC.Prim (StackSnapshot#, cloneMyStack#, ThreadId#, MutableArray#, sizeofMutableArray#, readArray#)
import GHC.Exts (Int(I#), RealWorld, Ptr(..))
import Control.Concurrent.MVar
import Control.Monad (forM)
import GHC.Conc.Sync
import GHC.Stable
import GHC.IO (IO(..))
import Foreign -- Foreign.Ptr
import GHC.Stack.CCS (InfoProvEnt, ipeProv, infoProvToStrings)

-- | A frozen snapshot of the state of an execution stack.
--
-- @since 2.16.0.0
data StackSnapshot = StackSnapshot !StackSnapshot#

{-
Note [Stack Cloning]
~~~~~~~~~~~~~~~~~~~~
"Cloning" a stack means that it's StgStack closure is copied including the
stack memory (stack[]). The stack pointer (sp) of the clone is adjusted to be
valid.

The clone is "offline"/"cold", i.e. it won't be evaluated any further. This is
useful for further analyses like stack unwinding or traversal.

There are two different ways to clone a stack:
1. By the corresponding thread via a primop call (cloneMyStack#).
2. By sending a RTS message (Messages.c) with a MVar to the corresponding
   thread and receiving the stack by taking it out of this MVar.

A StackSnapshot# is really a pointer to an immutable StgStack closure with
the invariant that stack->sp points to a valid frame.
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

-- TODO: Cannot use `import GHC.Exts.Heap (StgInfoTable(..))` -> hidden package
type InfoTableCode = Word

foreign import ccall "decodeClonedStack" decodeClonedStack:: StackSnapshot# -> MutableArray# RealWorld (Ptr InfoTableCode)

foreign import ccall "lookupIPE" lookupIPE:: Ptr Word -> IO (Ptr InfoProvEnt)

decode :: StackSnapshot -> IO [[String]]
decode (StackSnapshot stack) = let
    array = decodeClonedStack stack
    arraySize = I# (sizeofMutableArray# array)
  in
    do
      forM [0 .. arraySize - 1] $ \(I# i) -> do
        v <- IO $ readArray# array i
        print $ "StgInfoTable->code : " ++ show (I# i) ++ " -- " ++ show v
        ipe <- lookupIPE v
        s <- (infoProvToStrings . ipeProv) ipe
        return s
