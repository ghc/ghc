{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- This module exposes an interface for capturing the state of a thread's
-- execution stack for diagnostics purposes.
--
-- @since 2.16.0.0
module GHC.Stack.CloneStack
  ( StackSnapshot (..),
    cloneMyStack,
    cloneThreadStack,
    decode,
    StackEntry (..),
  )
where

import Control.Concurrent.MVar
import Control.Monad (forM)
-- Foreign.Ptr

import Data.Maybe (catMaybes)
import Foreign
import GHC.Conc.Sync
import GHC.Exts (Int (I#), RealWorld)
import GHC.IO (IO (..))
import GHC.Prim (MutableArray#, StackSnapshot#, ThreadId#, cloneMyStack#, readArray#, sizeofMutableArray#)
import GHC.Stack.CCS (InfoProv (..), InfoProvEnt, ipeProv, peekInfoProv)

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

{-
Note [Stack Decoding]
~~~~~~~~~~~~~~~~~~~~~
A cloned stack is decoded (unwound) by looking up the Info Table Provenance
Entries (IPE) for every stack frame with `lookupIPE` in the RTS.

The relevant notes are:
  - Note [Mapping Info Tables to Source Positions]
  - Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]

The IPEs contain source locations and are here pulled from the RTS/C world into Haskell.
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

-- | Represents an Info Table in the RTS.
-- It cannot be instantiated because it's only used as a token.
data InfoTable

foreign import ccall "decodeClonedStack" decodeClonedStack :: StackSnapshot# -> MutableArray# RealWorld (Ptr InfoTable)

foreign import ccall "lookupIPE" lookupIPE :: Ptr InfoTable -> IO (Ptr InfoProvEnt)

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
-- The stacktrace is created from return frames with according 'InfoProv'
-- entries. To generate them, use the GHC flag @-finfo-table-map@. If there are
-- no 'InfoProv' entries, an empty list is returned.
--
-- @since 2.16.0.0
decode :: StackSnapshot -> IO [StackEntry]
decode (StackSnapshot stack) =
  let array = decodeClonedStack stack
      arraySize = I# (sizeofMutableArray# array)
   in do
        result <- forM [0 .. arraySize - 1] $ \(I# i) -> do
          v <- IO $ readArray# array i
          ipe <- lookupIPE v
          if ipe == nullPtr
            then pure Nothing
            else do
              infoProv <- (peekInfoProv . ipeProv) ipe
              pure $ Just (toStackEntry infoProv)

        return $ catMaybes result
  where
    toStackEntry :: InfoProv -> StackEntry
    toStackEntry infoProv =
      StackEntry
        { functionName = ipLabel infoProv,
          moduleName = ipMod infoProv,
          srcLoc = ipLoc infoProv,
          -- read looks dangerous, be we can trust that the closure type is always there.
          closureType = read . ipDesc $ infoProv
        }
