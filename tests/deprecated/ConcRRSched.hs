{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.Schedulers.ConcRRSched
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


module ConcRRSched
(ConcRRSched
, SContStatus
, newConcRRSched      -- IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, forkOS              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, newVProc            -- ConcRRSched -> IO ()
) where

import LwConc.Substrate
import qualified System.Exit as E
import qualified Data.Sequence as Seq
import qualified Control.Concurrent as Conc
-- import qualified Control.OldException as Exn

data ConcRRSched = ConcRRSched (PVar (Seq.Seq SCont)) (PVar Int)

newConcRRSched :: IO (ConcRRSched)
newConcRRSched = do
  ref <- newPVarIO Seq.empty
  token <- newPVarIO 0
  s <- getSContIO
  (b,u) <- getSchedActionPairPrim (ConcRRSched ref token)
  setUnblockThread s $ u s
  setSwitchToNextThread s b
   -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  return $ ConcRRSched ref token

newVProc :: ConcRRSched -> IO ()
newVProc sched = do
  let loop = do {
    yield sched;
    loop
  }
  s <- newBoundSCont $ print "Running VProc" >> loop
  (b,u) <- getSchedActionPairPrim sched;
  setUnblockThread s $ u s;
  setSwitchToNextThread s b;
  scheduleSContOnFreeCap s

switchToNextAndFinish :: ConcRRSched -> IO ()
switchToNextAndFinish (ConcRRSched ref token) =
  let body = do {
  contents <- readPVar ref;
  case contents of
       (Seq.viewl -> Seq.EmptyL) -> undefined
       (Seq.viewl -> x Seq.:< tail) -> do
          canRun <- iCanRunSCont x
          if canRun
            then do {
              writePVar ref $ tail;
              setSContSwitchReason Completed;
              switchTo x
            }
            else do {
              enque (ConcRRSched ref token) x;
              body
            }
  }
  in atomically $ body

data SContKind = Bound | Unbound

fork :: ConcRRSched -> IO () -> SContKind -> IO ()
fork (ConcRRSched ref token) task kind = do
  let yieldingTask = do {
    {-Exn.try-} task;
    switchToNextAndFinish (ConcRRSched ref token);
    print "ConcRRSched.forkIO: Should not see this!"
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  s <- makeSCont yieldingTask;
  (b,u) <- getSchedActionPairPrim (ConcRRSched ref token);
  setUnblockThread s $ u s;
  setSwitchToNextThread s b;
  t <- atomically $ readPVar token
  nc <- Conc.getNumCapabilities
  setOC s t
  -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> s
    writePVar token $ (t + 1) `mod` nc

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO sched task = fork sched task Unbound


forkOS :: ConcRRSched -> IO () -> IO ()
forkOS sched task = fork sched task Bound

switchToNextWith :: ConcRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> PTM ()
switchToNextWith (ConcRRSched ref tok) f = do
  contents <- readPVar ref;
  case f contents of
      (Seq.viewl -> Seq.EmptyL) -> undefined
      (Seq.viewl -> x Seq.:< tail) -> do
        canRun <-iCanRunSCont x
        if canRun
          then do {
            writePVar ref $ tail;
            switchTo x
          }
          else do {
            enque (ConcRRSched ref tok) x;
            switchToNextWith (ConcRRSched ref tok) (\x -> x)
          }

enque :: ConcRRSched -> SCont -> PTM ()
enque (ConcRRSched ref _) s = do
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq


yield :: ConcRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  setSContSwitchReason Yielded
  switchToNextWith sched (\tail -> tail Seq.|> s)

getSchedActionPairPrim :: ConcRRSched -> IO (PTM (), SCont -> PTM ())
getSchedActionPairPrim sched = do
  let blockAction = switchToNextWith sched (\tail -> tail)
  let unblockAction s = do
      enque sched s
  return (blockAction, unblockAction)
