{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FairShare
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


module FairShare
(
  Sched
, SCont

, newSched           -- IO (Sched)
, newCapability      -- IO ()
, forkIO             -- IO () -> IO SCont
, forkOS             -- IO () -> IO SCont
, forkOn             -- Int -> IO () -> IO SCont
, yield              -- IO ()

, throwTo            -- Exception e => SCont -> e -> IO ()
, BlockedIndefinitelyOnConcDS(..)
, blockedIndefinitelyOnConcDS
) where

import System.Time
import LwConc.Substrate
import Data.Array.IArray
import Data.Dynamic
import Control.Monad
import Data.Maybe
import qualified Data.PQueue.Min as PQ

#include "profile.h"

newtype State = State (PVar Int, PVar ClockTime, PVar Int)
                deriving (Typeable)

-------------------------------------------------------------------------------
-- SCont Accounting
-------------------------------------------------------------------------------

-- |Returns the time difference in microseconds (potentially returning maxBound
-- <= the real difference)
timeDiffToMicroSec :: TimeDiff -> Int
timeDiffToMicroSec (TimeDiff _ _ _ _ _ sec picosec) =
    if realTime > fromIntegral (maxBound :: Int)
        then maxBound
  else fromIntegral realTime
  where
  realTime :: Integer
  realTime = (fromIntegral sec) * (10^6) + fromIntegral (picosec `div` (10^6))

_INL_(startClock)
startClock :: SCont -> PTM ()
startClock sc = do
  sls <- getSLS sc
  let State (_,st,_) = fromJust $ fromDynamic sls
  time <- unsafeIOToPTM $ getClockTime
  writePVar st $ time

_INL_(stopClock)
stopClock :: SCont -> PTM ()
stopClock sc = do
  sls <- getSLS sc
  let State (_,st,acc) = fromJust $ fromDynamic sls
  startTime <- readPVar st
  endTime <- unsafeIOToPTM $ getClockTime
  sum <- readPVar acc
  let newSum = sum + timeDiffToMicroSec (diffClockTimes endTime startTime)
  writePVar acc newSum
  where

-------------------------------------------------------------------------------
-- Scheduler
-------------------------------------------------------------------------------

data Elem = Elem SCont Int deriving Eq

instance Ord Elem where
  compare (Elem _ a) (Elem _ b) = compare a b

newtype Sched = Sched (Array Int (PVar (PQ.MinQueue Elem)))

emptyScheduler :: Int -> IO Sched
emptyScheduler numCaps = do
  l <- replicateM numCaps $ newPVarIO PQ.empty
  return $ Sched $ listArray (0, numCaps-1) l

_INL_(deque)
deque :: Sched -> SCont -> PTM (Maybe SCont)
deque !(Sched pa) !sc = do
  cc <- getSContCapability sc
  pq <- readPVar $! pa ! cc
  case PQ.getMin pq of
    Nothing -> return $ Nothing
    Just (Elem x _) -> do
      writePVar (pa ! cc) (PQ.deleteMin pq)
      return $ Just x

_INL_(enque)
enque :: Sched -> SCont -> PTM ()
enque !(Sched pa) !sc = do
  cc <- getSContCapability sc
  pq <- readPVar $! pa ! cc
  acc <- readAcc sc
  let newPq = PQ.insert (Elem sc acc) pq
  writePVar (pa ! cc) newPq
  where
    readAcc sc = do
      sls <- getSLS sc
      let State (_,_,acc) = fromJust $ fromDynamic sls
      readPVar acc



-------------------------------------------------------------------------------
-- Scheduler Activations
-------------------------------------------------------------------------------

_INL_(yieldControlAction)
yieldControlAction :: Sched -> SCont -> PTM ()
yieldControlAction !sched !sc = do
  -- Accounting
  stopClock sc
  -- Switch to next thread
  maybeSC <- deque sched sc
  case maybeSC of
    Nothing -> sleepCapability
    Just x -> startClock x >> switchTo x


_INL_(scheduleSContAction)
scheduleSContAction :: Sched -> SCont -> PTM ()
scheduleSContAction !sched !sc = do
  -- Since we are making the given scont runnable, update its status to
  -- Yielded.
  setSContSwitchReason sc Yielded
  enque sched sc

-------------------------------------------------------------------------------
-- External scheduler interface
-------------------------------------------------------------------------------

_INL_(newSched)
newSched :: IO (Sched)
newSched = do

  -- This token will be used to spawn in a round-robin fashion on different
  -- capabilities.
  token <- newPVarIO (0::Int)
  -- Start time
  startTime <- getClockTime >>= newPVarIO
  -- Time diff
  timeDiff <- newPVarIO (0::Int)
  -- Save the token in the Thread-local State (TLS)
  s <- getSContIO
  setSLS s $ toDyn $ State (token, startTime, timeDiff)

  -- Create the scheduler data structure
  nc <- getNumCapabilities
  sched <- emptyScheduler nc

  -- Initialize scheduler actions
  atomically $ do {
  setYieldControlAction s $ yieldControlAction sched;
  setScheduleSContAction s $ scheduleSContAction sched
  }
  -- return scheduler
  return sched


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

  -- SLS
  token <- newPVarIO (0::Int)
  startTime <- getClockTime >>= newPVarIO
  timeDiff <- newPVarIO (0::Int)
  let state = State (token, startTime, timeDiff)
  setSLS s $ toDyn state

  atomically $ do
    mySC <- getSCont
    yca <- getYieldControlActionSCont mySC
    setYieldControlAction s yca
    ssa <- getScheduleSContActionSCont mySC
    setScheduleSContAction s ssa
  scheduleSContOnFreeCap s

data SContKind = Bound | Unbound

_INL_(fork)
fork :: IO () -> Maybe Int -> SContKind -> IO SCont
fork task on kind = do
  currentSC <- getSContIO
  nc <- getNumCapabilities

  -- epilogue: Switch to next thread after completion
  let epilogue = atomically $ do {
    sc <- getSCont;
    setSContSwitchReason sc Completed;
    switchToNext <- getYieldControlAction;
    switchToNext;
    return ()
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  newSC <- makeSCont (task >> epilogue)

  -- Initialize TLS
  tls <- atomically $ getSLS currentSC
  let State (token, _, _) = fromJust $ fromDynamic tls
  -- Start time
  startTime <- getClockTime >>= newPVarIO
  -- Time diff
  timeDiff <- newPVarIO (0::Int)
  setSLS newSC $ toDyn $ State (token, startTime, timeDiff)

  t <- atomically $ do
    mySC <- getSCont
    -- Initialize scheduler actions
    yca <- getYieldControlActionSCont mySC
    setYieldControlAction newSC yca
    ssa <- getScheduleSContActionSCont mySC
    setScheduleSContAction newSC ssa
    t <- readPVar token
    writePVar token $ (t+1) `mod` nc
    return t

  -- Set SCont Affinity
  case on of
    Nothing -> setSContCapability newSC t
    Just t' -> setSContCapability newSC $ t' `mod` nc

  -- Schedule new Scont
  atomically $ do
    ssa <- getScheduleSContActionSCont newSC
    ssa newSC
  return newSC

_INL_(forkIO)
forkIO :: IO () -> IO SCont
forkIO task = fork task Nothing Unbound

_INL_(forkOS)
forkOS :: IO () -> IO SCont
forkOS task = fork task Nothing Bound

_INL_(forkOn)
forkOn :: Int -> IO () -> IO SCont
forkOn on task = fork task (Just on) Unbound


_INL_(yield)
yield :: IO ()
yield = atomically $ do
  -- Update SCont status to Yielded
  s <- getSCont
  setSContSwitchReason s Yielded
  -- Append current SCont to scheduler
  append <- getScheduleSContAction
  append
  -- Switch to next SCont from Scheduler
  switchToNext <- getYieldControlAction
  switchToNext
