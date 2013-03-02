{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

module LwConc.MVar
(
  MVar
, newMVar       -- a -> IO (MVar a)
, newEmptyMVar  -- IO (MVar a)

, putMVar       -- MVar a -> a -> IO ()
, asyncPutMVar  -- MVar a -> a -> PTM ()
, takeMVar      -- MVar a -> IO a

, readMVar      -- MVar a -> a
, swapMVar      -- MVar a -> a -> IO a
) where

import LwConc.Substrate
import qualified Data.Sequence as Seq
import GHC.IORef

newtype MVar a = MVar (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full a (Seq.Seq (a, PTM()))
                | Empty (Seq.Seq (IORef a, PTM()))


newMVar :: a -> IO (MVar a)
newMVar x = do
  ref <- newPVarIO $ Full x Seq.empty
  return $ MVar ref

newEmptyMVar :: IO (MVar a)
newEmptyMVar = do
  ref <- newPVarIO $ Empty Seq.empty
  return $ MVar ref

{-# INLINE readMVar #-}
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
           writePVar ref $ Empty $ ts Seq.|> (hole, wakeup)
           setSContSwitchReason sc $ BlockedInHaskell token
           blockAct
         Full x _ -> unsafeIOToPTM $ writeIORef hole x
  readIORef hole

{-# INLINE swapMVar #-}
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
             oldValue <- unsafeIOToPTM $ readIORef hole;
             -- put value back into the MVar
             putMVarPTM (MVar ref) newValue;
             -- Should I resume?
             v <- isResumeTokenValid token;
             if v then
               unblockAct sc
             else
               return ()
           }
           writePVar ref $ Empty $ ts Seq.|> (hole, wakeup)
           setSContSwitchReason sc $ BlockedInHaskell token
           blockAct
         Full x _ -> unsafeIOToPTM $ writeIORef hole x
  readIORef hole


{-# INLINE asyncPutMVar #-}
asyncPutMVar :: MVar a -> a -> PTM ()
asyncPutMVar (MVar ref) x = do
  st <- readPVar ref
  case st of
       Empty (Seq.viewl -> Seq.EmptyL) -> do
         writePVar ref $ Full x Seq.empty
       Empty (Seq.viewl -> (hole, wakeup) Seq.:< ts) -> do
         unsafeIOToPTM $ writeIORef hole x
         writePVar ref $ Empty ts
         wakeup
       Full x' ts -> do
         writePVar ref $ Full x' $ ts Seq.|> (x, return ())


{-# INLINE putMVarPTM #-}
putMVarPTM :: MVar a -> a -> PTM ()
putMVarPTM (MVar ref) x = do
  st <- readPVar ref
  case st of
       Empty (Seq.viewl -> Seq.EmptyL) -> do
         writePVar ref $ Full x Seq.empty
       Empty (Seq.viewl -> (hole, wakeup) Seq.:< ts) -> do
         unsafeIOToPTM $ writeIORef hole x
         writePVar ref $ Empty ts
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
         writePVar ref $ Full x' $ ts Seq.|> (x, wakeup)
         setSContSwitchReason sc $ BlockedInHaskell token
         blockAct

{-# INLINE putMVar #-}
putMVar :: MVar a -> a -> IO ()
putMVar mv x = atomically $ putMVarPTM mv x


{-# INLINE takeMVar #-}
takeMVar :: MVar a -> IO a
takeMVar (MVar ref) = do
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
             v <- isResumeTokenValid token;
             if v then
               unblockAct sc
             else
               return ()
           }
           writePVar ref $ Empty $ ts Seq.|> (hole, wakeup)
           setSContSwitchReason sc $ BlockedInHaskell token
           blockAct
         Full x (Seq.viewl -> Seq.EmptyL) -> do
           writePVar ref $ Empty Seq.empty
           unsafeIOToPTM $ writeIORef hole x
         Full x (Seq.viewl -> (x', wakeup) Seq.:< ts) -> do
           writePVar ref $ Full x' ts
           unsafeIOToPTM $ writeIORef hole x
           wakeup
  readIORef hole
