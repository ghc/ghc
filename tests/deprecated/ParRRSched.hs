{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.Schedulers.ParRRSched
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


module ParRRSched
(ParRRSched
, SContStatus
, newParRRSched      -- IO (ParRRSched)
, forkIO              -- ParRRSched -> IO () -> IO ()
, forkOS              -- ParRRSched -> IO () -> IO ()
, yield               -- ParRRSched -> IO ()
, newVProc            -- ParRRSched -> IO ()
) where

import LwConc.Substrate
import qualified System.Exit as E
import qualified Data.Sequence as Seq
import Data.Array.IArray
import qualified Control.Concurrent as Conc
-- import qualified Control.OldException as Exn

data ParRRSched = ParRRSched (Array Int (PVar (Seq.Seq SCont))) (PVar Int)

newParRRSched :: IO (ParRRSched)
newParRRSched = do
  token <- newPVarIO 0
  s <- getSContIO
  nc <- Conc.getNumCapabilities
  rl <- createPVarList nc []
  let sched = ParRRSched (listArray (0, nc-1) rl) token
  (b,u) <- getSchedActionPairPrim sched
  setUnblockThread s u
  setSwitchToNextThread s b
   -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  return sched

createPVarList 0 l = return l
createPVarList n l = do {
  ref <- newPVarIO Seq.empty;
  createPVarList (n-1) $ ref:l
}

newVProc :: ParRRSched -> IO ()
newVProc sched = do
  let ParRRSched pa _ = sched
  let body = do {
    s <- getSCont;
    cc <- getCurrentCapability;
    contents <- readPVar $ pa ! cc;
    case contents of
      (Seq.viewl -> Seq.EmptyL) -> do
        sleepCapability
      (Seq.viewl -> x Seq.:< tail) -> do
        setSContSwitchReason Yielded
        writePVar (pa ! cc) $ tail Seq.|> s
        switchTo x
  }
  let loop = do {
    atomically $ body;
    loop
  }
  s <- newSCont loop
  (b,u) <- getSchedActionPairPrim sched;
  setUnblockThread s u;
  setSwitchToNextThread s b;
  scheduleSContOnFreeCap s

switchToNextAndFinish :: ParRRSched -> IO ()
switchToNextAndFinish (ParRRSched pa _) = atomically $ do
  cc <- getCurrentCapability
  let ref = pa ! cc
  contents <- readPVar ref
  case contents of
      (Seq.viewl -> Seq.EmptyL) -> do
        sleepCapability
      (Seq.viewl -> x Seq.:< tail) -> do
        writePVar ref $ tail
        setSContSwitchReason Completed
        switchTo x

data SContKind = Bound | Unbound

fork :: ParRRSched -> IO () -> SContKind -> IO ()
fork sched task kind = do
  let ParRRSched pa token = sched
  let yieldingTask = do {
    {-Exn.try-} task;
    switchToNextAndFinish sched;
    print "ParRRSched.forkIO: Should not see this!"
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  s <- makeSCont yieldingTask;
  (b,u) <- getSchedActionPairPrim sched;
  setUnblockThread s u;
  setSwitchToNextThread s b;
  t <- atomically $ readPVar token
  nc <- Conc.getNumCapabilities
  cc <- atomically $ getCurrentCapability
  let ref = pa ! t
  setSContCapability s t
  -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> s
    writePVar token $ (t + 1) `mod` nc

forkIO :: ParRRSched -> IO () -> IO ()
forkIO sched task = fork sched task Unbound


forkOS :: ParRRSched -> IO () -> IO ()
forkOS sched task = fork sched task Bound

switchToNextWith :: ParRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> PTM ()
switchToNextWith sched f = do
  let ParRRSched pa _ = sched
  cc <- getCurrentCapability
  let ref = pa ! cc
  contents <- readPVar ref;
  case f contents of
      (Seq.viewl -> Seq.EmptyL) -> do
        sleepCapability
      (Seq.viewl -> x Seq.:< tail) -> do
        writePVar ref tail
        switchTo x

enque :: ParRRSched -> SCont -> PTM ()
enque (ParRRSched pa _) s = do
  sc <- getSContCapability s
  let ref = pa ! sc
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq


yield :: ParRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  setSContSwitchReason Yielded
  switchToNextWith sched (\tail -> tail Seq.|> s)

getSchedActionPairPrim :: ParRRSched -> IO (PTM (), SCont -> PTM ())
getSchedActionPairPrim sched = do
  let blockAction = switchToNextWith sched (\tail -> tail)
  let unblockAction s = do
      id <- getSContId s
      enque sched s
  return (blockAction, unblockAction)
