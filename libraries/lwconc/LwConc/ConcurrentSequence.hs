{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.Schedulers.ConcurrentSequence
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A concurrent round-robin scheduler.
--
-----------------------------------------------------------------------------


module LwConc.ConcurrentSequence
(
  Sched
, SCont

, newSched           -- IO (Sched)
, newCapability      -- IO ()
, forkIO             -- IO () -> IO SCont
, forkOS             -- IO () -> IO SCont
, yield              -- IO ()

, throwTo            -- Exception e => SCont -> e -> IO ()
, BlockedIndefinitelyOnConcDS(..)
, blockedIndefinitelyOnConcDS
) where

import LwConc.Substrate
import Data.Sequence
import Data.Array.IArray
import Data.Dynamic

#define _INL_(x) {-# INLINE x #-}

-- The scheduler data structure has one (PVar (Seq SCont)) for every capability.
newtype Sched = Sched (Array Int (PVar (Seq SCont)))

_INL_(yieldControlAction)
yieldControlAction :: Sched -> PTM ()
yieldControlAction (Sched pa) = do
  -- Fetch current capability's scheduler
  cc <- getCurrentCapability
  let ref = pa ! cc
  contents <- readPVar ref
  -- If there are no runnable tasks, sleep. Otherwise, switch to the runnable
  -- task from the head of the sequence.
  case contents of
    (viewl -> EmptyL) -> do
      sleepCapability
    (viewl -> x :< tl) -> do
      writePVar ref tl
      switchTo x

_INL_(scheduleSContAction)
scheduleSContAction :: Sched -> SCont -> PTM ()
scheduleSContAction (Sched pa) sc = do
  -- Since we are making the given scont runnable, update its status to Yielded.
  setSContSwitchReason sc Yielded
  -- Fetch the given SCont's scheduler.
  cap <- getSContCapability sc
  let ref = pa ! cap
  contents <- readPVar ref
  -- Append the given task to the tail.
  writePVar ref $ contents |> sc


_INL_(newSched)
newSched :: IO (Sched)
newSched = do
  -- This token will be used to spawn in a round-robin fashion on different
  -- capabilities.
  token <- newPVarIO (0::Int)
  -- Save the token in the Thread-local State (TLS)
  s <- getSContIO
  setSLS s $ toDyn token
  -- Create the scheduler data structure
  nc <- getNumCapabilities
  rl <- createPVarList nc []
  let sched = Sched (listArray (0, nc-1) rl)
  -- Initialize scheduler actions
  atomically $ do {
  setYieldControlAction s $ yieldControlAction sched;
  setScheduleSContAction s $ scheduleSContAction sched
  }
  -- return scheduler
  return sched
  where
    createPVarList 0 l = return l
    createPVarList n l = do {
      ref <- newPVarIO empty;
      createPVarList (n-1) $ ref:l
    }

_INL_(newCapability)
newCapability :: IO ()
newCapability = do
 -- Initial task body
 let initTask = atomically $ do {
  s <- getSCont;
  yca <- getYieldControlAction;
  setSContSwitchReason s Completed;
  yca
 }
 -- Create and initialize new task
 s <- newSCont initTask
 atomically $ do {
   yca <- getYieldControlAction;
   setYieldControlAction s yca;
   ssa <- getScheduleSContAction;
   setScheduleSContAction s ssa
 }
 scheduleSContOnFreeCap s

data SContKind = Bound | Unbound

_INL_(fork)
fork :: IO () -> SContKind -> IO SCont
fork task kind = do
  currentSC <- getSContIO
  nc <- getNumCapabilities
  -- epilogue: Switch to next thread after completion
  let epilogue = atomically $ do {
    sc <- getSCont;
    setSContSwitchReason sc Completed;
    switchToNext <- getYieldControlAction;
    switchToNext
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  newSC <- makeSCont (task >> epilogue)
  -- Initialize TLS
  tls <- atomically $ getSLS currentSC
  setSLS newSC $ tls
  let token::PVar Int = case fromDynamic tls of
                          Nothing -> error "TLS"
                          Just x -> x
  t <- atomically $ do {
    -- Initialize scheduler actions
    yca <- getYieldControlAction;
    setYieldControlAction newSC yca;
    ssa <- getScheduleSContAction;
    setScheduleSContAction newSC ssa;
    t <- readPVar token;
    writePVar token $ (t+1) `mod` nc;
    return t
  }
  -- Set SCont Affinity
  setSContCapability newSC t
  -- Schedule new Scont
  atomically $ do {
    ssa <- getScheduleSContAction;
    ssa newSC
  }
  return newSC

_INL_(forkIO)
forkIO :: IO () -> IO SCont
forkIO task = fork task Unbound

_INL_(forkOS)
forkOS :: IO () -> IO SCont
forkOS task = fork task Bound

_INL_(yield)
yield :: IO ()
yield = atomically $ do
  s <- getSCont
  -- Append current SCont to scheduler
  ssa <- getScheduleSContAction
  let append = ssa s
  append
  -- Switch to next SCont from Scheduler
  switchToNext <- getYieldControlAction
  switchToNext
