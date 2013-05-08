{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- An implementation of MVar. This implementation is scheduler agnostic.
--
-----------------------------------------------------------------------------

module MVarList
(
  MVar
, newMVar       -- a -> IO (MVar a)
, newEmptyMVar  -- IO (MVar a)

, putMVar       -- MVar a -> a -> IO ()
, asyncPutMVar  -- MVar a -> a -> PTM ()
, takeMVar      -- MVar a -> IO a
, takeMVarWithHole -- MVar a -> IORef a -> IO a
) where

import LwConc.Substrate
import GHC.IORef

#include "profile.h"

-- data Queue a = Queue ![a] ![a]
-- 
-- _INL_(emptyQueue)
-- emptyQueue :: Queue a
-- emptyQueue = Queue [] []
-- 
-- _INL_(enque)
-- enque :: Queue a -> a -> Queue a
-- enque (Queue front back) e = Queue front $ e:back
-- 
-- _INL_(deque)
-- deque :: Queue a -> (Queue a, Maybe a)
-- deque (Queue !front !back) =
--   case front of
--     [] -> (case reverse back of
--             [] -> (emptyQueue, Nothing)
--             x:tl -> (Queue tl [], Just x))
--     x:tl -> (Queue tl back, Just x)

-- NOTE KC: Even a list seems to work just as well as a queue.
data Queue a = Queue [a]

_INL_(emptyQueue)
emptyQueue :: Queue a
emptyQueue = Queue []

_INL_(enque)
enque :: Queue a -> a -> Queue a
enque (Queue q) e = Queue $! e:q

_INL_(deque)
deque :: Queue a -> (Queue a, Maybe a)
deque (Queue q) =
  case q of
    [] -> (emptyQueue, Nothing)
    x:tl -> (Queue tl, Just x)

newtype MVar a = MVar (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full !a (Queue (a, PTM()))
                | Empty (Queue (IORef a, PTM()))


_INL_(newMVar)
newMVar :: a -> IO (MVar a)
newMVar x = do
  ref <- newPVarIO $! Full x emptyQueue
  return $! MVar ref

_INL_(newEmptyMVar)
newEmptyMVar :: IO (MVar a)
newEmptyMVar = do
  ref <- newPVarIO $! Empty emptyQueue
  return $! MVar ref


_INL_(asyncPutMVar)
asyncPutMVar :: MVar a -> a -> PTM ()
asyncPutMVar (MVar ref) x = do
  st <- readPVar ref
  case st of
       Empty ts ->
         case deque ts of
           (_, Nothing) -> do
             writePVar ref $! Full x emptyQueue
           (ts', Just (hole, wakeup)) -> do
             unsafeIOToPTM $! writeIORef hole x
             writePVar ref $! Empty ts'
             wakeup
       Full x' ts -> do
         writePVar ref $ Full x' $ enque ts (x, return ())


_INL_(putMVarPTM)
putMVarPTM :: MVar a -> a -> ResumeToken -> PTM ()
putMVarPTM (MVar ref) x token = do
  st <- readPVar ref
  case st of
       Empty ts -> do
         case deque ts of
           (_, Nothing) -> do
             writePVar ref $! Full x emptyQueue
           (ts', Just (hole, wakeup)) -> do
             unsafeIOToPTM $! writeIORef hole x
             writePVar ref $! Empty ts'
             wakeup
       Full x' ts -> do
         blockAct <- getYieldControlAction
         sc <- getSCont
         unblockAct <- getScheduleSContAction
         let !wakeup = unblockAct
         writePVar ref $! Full x' $! enque ts (x, wakeup)
         setSContSwitchReason sc $! BlockedInHaskell token
         blockAct

_INL_(putMVar)
putMVar :: MVar a -> a -> ResumeToken -> IO ()
putMVar mv x token = atomically $ putMVarPTM mv x token

_INL_(takeMVarWithHole)
takeMVarWithHole :: MVar a -> IORef a -> ResumeToken -> IO a
takeMVarWithHole (MVar ref) hole token = do
  atomically $ do
    st <- readPVar ref
    case st of
      Empty ts -> do
        blockAct <- getYieldControlAction
        sc <- getSCont
        unblockAct <- getScheduleSContAction
        let !wakeup = unblockAct
        writePVar ref $! Empty $! enque ts (hole, wakeup)
        setSContSwitchReason sc $! BlockedInHaskell token
        blockAct
      Full x ts -> do
        case deque ts of
          (_, Nothing) -> do
            writePVar ref $! Empty emptyQueue
            unsafeIOToPTM $! writeIORef hole x
          (ts', Just (x', wakeup)) -> do
            writePVar ref $! Full x' ts'
            unsafeIOToPTM $! writeIORef hole x
            wakeup
  readIORef hole

_INL_(takeMVar)
takeMVar :: MVar a -> ResumeToken -> IO a
takeMVar m token = do
  hole <- newIORef undefined
  takeMVarWithHole m hole token
