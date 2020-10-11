{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes#-}


module GHC.Stack.CloneStack (
  cloneThreadStack,
  cloneMyStack,
  printStack,
  StackSnapshot(..)
  ) where

import GHC.Prim (StackSnapshot#, cloneMyStack#, ThreadId#)
import Control.Concurrent.MVar
import GHC.Conc.Sync
import GHC.Stable
import GHC.IO (IO(..))

foreign import ccall "sendCloneStackMessage" sendCloneStackMessage :: ThreadId# -> StablePtr PrimMVar -> IO ()

foreign import ccall "PrinterAPI.h printStack" printStack_c :: StackSnapshot# -> IO ()

data StackSnapshot = StackSnapshot StackSnapshot#

{- Note [Stack Cloning]
"Cloning" a stack means that it's StgStack closure is copied including the
stack memory (stack[]). The stack pointer (sp) of the clone is adjusted to be
valid.
The clone is "offline"/"cold", i.e. it won't be evaluated any further. This is
useful for further analyses like stack unwinding or traversal.

There are two different ways to clone a stack:
1. By the corresponding thread via a primop call (cloneMyStack#).
2. By sending a RTS message (Messages.c) with a MVar to the corresponding
   thread and receiving the stack by taking it out of this MVar.
-}

-- | Clone the stack of a thread identified by it's 'ThreadId'
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

-- | Clone the stack of the executing thread
cloneMyStack :: IO StackSnapshot
cloneMyStack = IO $ \s ->
   case (cloneMyStack# s) of (# s1, stack #) -> (# s1, StackSnapshot stack #)

-- | Print the stack
printStack :: StackSnapshot -> IO ()
printStack (StackSnapshot stack) = printStack_c stack
