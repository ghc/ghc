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

module LwConc.MVarList
(
  MVar
, newMVar       -- a -> IO (MVar a)
, newEmptyMVar  -- IO (MVar a)

, putMVar       -- MVar a -> a -> IO ()
, asyncPutMVar  -- MVar a -> a -> PTM ()
, takeMVar      -- MVar a -> IO a
, takeMVarWithHole -- MVar a -> IORef a -> IO a

, readMVar      -- MVar a -> a
, swapMVar      -- MVar a -> a -> IO a
) where

import LwConc.Substrate
import GHC.IORef

#define _INL_(x) {-# INLINE x #-}

data TwoListQueue a = TwoListQueue [a] [a]

_INL_(emptyQueue)
emptyQueue :: TwoListQueue a
emptyQueue = TwoListQueue [] []

_INL_(enque)
enque :: TwoListQueue a -> a -> TwoListQueue a
enque (TwoListQueue front back) e = TwoListQueue front $ e:back

_INL_(deque)
deque :: TwoListQueue a -> (TwoListQueue a, Maybe a)
deque (TwoListQueue front back) =
  case front of
    [] -> (case reverse back of
            [] -> (emptyQueue, Nothing)
            x:tl -> (TwoListQueue tl [], Just x))
    x:tl -> (TwoListQueue tl back, Just x)

newtype MVar a = MVar (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full a (TwoListQueue (a, PTM()))
                | Empty (TwoListQueue (IORef a, PTM()))


_INL_(newMVar)
newMVar :: a -> IO (MVar a)
newMVar x = do
  ref <- newPVarIO $ Full x emptyQueue
  return $ MVar ref

_INL_(newEmptyMVar)
newEmptyMVar :: IO (MVar a)
newEmptyMVar = do
  ref <- newPVarIO $ Empty emptyQueue
  return $ MVar ref

_INL_(readMVar)
readMVar :: MVar a -> IO a
readMVar (MVar ref) = do
  hole <- newIORef undefined
  atomically $ do
    st <- readPVar ref
    case st of
         Empty ts -> do
           blockAct <- getYieldControlAction
           sc <- getSCont
           unblockAct <- getScheduleSContAction
           token <- newResumeToken
           let wakeup = do {
             value <- unsafeIOToPTM $ readIORef hole;
             -- put value back into the MVar
             putMVarPTM (MVar ref) value;
             -- Should I resume?
             v <- isResumeTokenValid token;
             if v then
               unblockAct sc
             else
               return ()
           }
           writePVar ref $ Empty $ enque ts (hole, wakeup)
           setSContSwitchReason sc $ BlockedInHaskell token
           blockAct
         Full x _ -> unsafeIOToPTM $ writeIORef hole x
  readIORef hole

_INL_(swapMVar)
swapMVar :: MVar a -> a -> IO a
swapMVar (MVar ref) newValue = do
  hole <- newIORef undefined
  atomically $ do
    st <- readPVar ref
    case st of
      Empty ts -> do
        blockAct <- getYieldControlAction
        sc <- getSCont
        unblockAct <- getScheduleSContAction
        token <- newResumeToken
        let wakeup = do {
          -- put new value into the MVar. MVar behavior assures that the
          -- MVar ref will be empty with 0 or more pending readers. Hence,
          -- this call wouldn't block.
          putMVarPTM (MVar ref) newValue;
          -- Should I resume?
          v <- isResumeTokenValid token;
          if v then
            unblockAct sc
          else
            return ()
        }
        writePVar ref $ Empty $ enque ts (hole, wakeup)
        setSContSwitchReason sc $ BlockedInHaskell token
        blockAct
      Full x ts -> do
        case deque ts of
          (_, Nothing) -> do
            -- First take the old value
            writePVar ref $ Empty emptyQueue
            unsafeIOToPTM $ writeIORef hole x
            -- Now put the new value in
            putMVarPTM (MVar ref) newValue
          (ts', Just (x', wakeup)) -> do
            -- First take the old value
            unsafeIOToPTM $ writeIORef hole x
            writePVar ref $ Full x' ts'
            wakeup
            -- Now put the new value in
            putMVarPTM (MVar ref) newValue
  readIORef hole


_INL_(asyncPutMVar)
asyncPutMVar :: MVar a -> a -> PTM ()
asyncPutMVar (MVar ref) x = do
  st <- readPVar ref
  case st of
       Empty ts ->
         case deque ts of
           (_, Nothing) -> do
             writePVar ref $ Full x emptyQueue
           (ts', Just (hole, wakeup)) -> do
             unsafeIOToPTM $ writeIORef hole x
             writePVar ref $ Empty ts'
             wakeup
       Full x' ts -> do
         writePVar ref $ Full x' $ enque ts (x, return ())


_INL_(putMVarPTM)
putMVarPTM :: MVar a -> a -> PTM ()
putMVarPTM (MVar ref) x = do
  st <- readPVar ref
  case st of
       Empty ts -> do
         case deque ts of
           (_, Nothing) -> do
             writePVar ref $ Full x emptyQueue
           (ts', Just (hole, wakeup)) -> do
             unsafeIOToPTM $ writeIORef hole x
             writePVar ref $ Empty ts'
             wakeup
       Full x' ts -> do
         blockAct <- getYieldControlAction
         sc <- getSCont
         unblockAct <- getScheduleSContAction
         token <- newResumeToken
         let wakeup = do {
           v <- isResumeTokenValid token;
           if v then
             unblockAct sc
           else
             return ()
         }
         writePVar ref $ Full x' $ enque ts (x, wakeup)
         setSContSwitchReason sc $ BlockedInHaskell token
         blockAct

_INL_(putMVar)
putMVar :: MVar a -> a -> IO ()
putMVar mv x = atomically $ putMVarPTM mv x

_INL_(takeMVarWithHole)
takeMVarWithHole :: MVar a -> IORef a -> IO a
takeMVarWithHole (MVar ref) hole = do
  atomically $ do
    st <- readPVar ref
    case st of
      Empty ts -> do
        blockAct <- getYieldControlAction
        sc <- getSCont
        unblockAct <- getScheduleSContAction
        token <- newResumeToken
        let wakeup = do {
          v <- isResumeTokenValid token;
          if v then
            unblockAct sc
          else
            return ()
        }
        writePVar ref $ Empty $ enque ts (hole, wakeup)
        setSContSwitchReason sc $ BlockedInHaskell token
        blockAct
      Full x ts -> do
        case deque ts of
          (_, Nothing) -> do
            writePVar ref $ Empty emptyQueue
            unsafeIOToPTM $ writeIORef hole x
          (ts', Just (x', wakeup)) -> do
            writePVar ref $ Full x' ts'
            unsafeIOToPTM $ writeIORef hole x
            wakeup
  readIORef hole

_INL_(takeMVar)
takeMVar :: MVar a -> IO a
takeMVar m = do
  hole <- newIORef undefined
  takeMVarWithHole m hole
