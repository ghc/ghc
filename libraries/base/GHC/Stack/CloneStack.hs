{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes#-}


module GHC.Stack.CloneStack (
  cloneThreadStack,
  cloneMyStack,
  StackSnapshot(..)
  ) where

import GHC.Prim (StackSnapshot#, cloneMyStack#, ThreadId#)
import Control.Concurrent.MVar
import GHC.Conc.Sync
import GHC.Stable
import GHC.IO (IO(..))

foreign import ccall "sendCloneStackMessage" sendCloneStackMessage :: ThreadId# -> StablePtr PrimMVar -> IO ()

data StackSnapshot = StackSnapshot StackSnapshot#

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
  print "takeMVar"
  takeMVar resultVar

-- | Clone the stack of the executing thread
cloneMyStack :: IO StackSnapshot
cloneMyStack = IO $ \s ->
   case (cloneMyStack# s) of (# s1, stack #) -> (# s1, StackSnapshot stack #)
