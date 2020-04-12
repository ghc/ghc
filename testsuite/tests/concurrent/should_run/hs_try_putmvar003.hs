{-# LANGUAGE MagicHash #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Foreign hiding (void)
import Foreign.C
import GHC.Conc
import GHC.MVar (MVar(..))
import GHC.Prim
import System.Environment
import System.Exit
import Unsafe.Coerce

-- Measure C to Haskell callback throughput under a workload with
-- several dimensions:
--
--  * X callback queues (each managed by an OS thread in C)
--  * each queue has Y Haskell threads, each making Z requests
--
-- And we can run the whole thing in two ways:
--  * With the callbacks calling into a foreign export
--  * With the callbacks using hs_try_putmvar()
--
-- Example results (using WAY=threaded2)
--
--  hs_try_putmvar003 1 64 16 500 +RTS -s -N4    1.10s
--  hs_try_putmvar003 2 64 16 500 +RTS -s -N4    9.88s
--
-- hs_try_putmvar() is 9x faster with these parameters.

main = do
   when (not rtsSupportsBoundThreads) $
     die "This test requires -threaded"
   args <- getArgs
   case args of
     ["1",x,y,z] -> experiment False (read x) (read y) (read z)
     ["2",x,y,z] -> experiment True (read x) (read y) (read z)

makeExternalCall :: Ptr CallbackQueue -> IO CInt
makeExternalCall q = mask_ $ do
  mvar <- newEmptyMVar
  sp <- newStablePtrPrimMVar mvar
  fp <- mallocForeignPtr
  (cap,_) <- threadCapability =<< myThreadId
  withForeignPtr fp $ \presult -> do
    scheduleCallback q sp cap presult
    takeMVar mvar `onException` forkIO (do takeMVar mvar; touchForeignPtr fp)
    peek presult

data CallbackQueue

foreign import ccall "mkCallbackQueue"
  mkCallbackQueue :: Int -> Int -> IO (Ptr CallbackQueue)

foreign import ccall "destroyCallbackQueue"
  destroyCallbackQueue :: Ptr CallbackQueue -> IO ()

foreign import ccall "scheduleCallback"
  scheduleCallback :: Ptr CallbackQueue
                   -> StablePtr PrimMVar
                   -> Int
                   -> Ptr CInt
                   -> IO ()

callbackPutMVar :: StablePtr PrimMVar -> IO ()
callbackPutMVar sp = do
  mvar <- deRefStablePtr sp
  void $ tryPutMVar (MVar (unsafeCoerce# mvar)) ()

foreign export ccall callbackPutMVar :: StablePtr PrimMVar -> IO ()

-- Make
--   * x callback queues, each with
--   * y threads, doing
--   * z requests each
experiment :: Bool -> Int -> Int -> Int -> IO ()
experiment use_foreign_export x y z = do
  mvars <- replicateM x $ async $ do
    bracket (mkCallbackQueue (fromEnum use_foreign_export) (z*y))
            destroyCallbackQueue $ \q -> do
      mvars <- replicateM y $ async $
        replicateM_ z $ void $ makeExternalCall q
      mapM_ takeMVar mvars
  mapM_ takeMVar mvars

async :: IO () -> IO (MVar ())
async io = do
  m <- newEmptyMVar
  forkFinally io (\_ -> putMVar m ())
  return m
