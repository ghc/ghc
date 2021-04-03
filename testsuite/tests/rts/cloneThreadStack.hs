{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim (StackSnapshot#, ThreadId#)
import GHC.Conc.Sync (ThreadId(..))
import GHC.Stack.CloneStack
import Control.Concurrent
import GHC.Conc
import System.Mem

foreign import ccall "expectStacksToBeEqual" expectStacksToBeEqual:: StackSnapshot# -> ThreadId# -> IO ()

foreign import ccall "expectStackToBeNotDirty" expectStackToBeNotDirty:: StackSnapshot# -> IO ()

-- | Clone the stack of another thread and check it's snapshot for being equal
-- with the live stack.
-- In the meanwhile enforce a garbage collection to ensure that the stack
-- snapshot is still valid afterwards (is not gc'ed while in use).
main :: IO ()
main = do
  mVarToBeBlockedOn <- newEmptyMVar
  threadId <- forkIO $ immediatelyBlocking mVarToBeBlockedOn

  waitUntilBlocked threadId

  stackSnapshot <- cloneThreadStack threadId

  performMajorGC

  let (StackSnapshot stack) = stackSnapshot
  let (ThreadId tid#) = threadId
  expectStacksToBeEqual stack tid#
  expectStackToBeNotDirty stack

immediatelyBlocking :: MVar Int -> IO ()
immediatelyBlocking mVarToBeBlockedOn = do
  takeMVar mVarToBeBlockedOn
  return ()

waitUntilBlocked :: ThreadId -> IO ()
waitUntilBlocked tid = do
  blocked <- isBlocked tid
  if blocked
    then return ()
    else do
      threadDelay 100000
      waitUntilBlocked tid

isBlocked :: ThreadId -> IO Bool
isBlocked = fmap isThreadStatusBlocked . threadStatus

isThreadStatusBlocked :: ThreadStatus -> Bool
isThreadStatusBlocked (ThreadBlocked _) = True
isThreadStatusBlocked _ = False
