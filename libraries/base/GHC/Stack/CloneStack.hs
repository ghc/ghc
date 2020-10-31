{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- This module exposes an interface for capturing the state of a thread's
-- execution stack for diagnostics purposes.
--
-- @since 2.16.0.0
module GHC.Stack.CloneStack (
  StackSnapshot(..),
  cloneMyStack
  ) where

import GHC.Prim (StackSnapshot#, cloneMyStack#)
import GHC.IO (IO(..))

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

