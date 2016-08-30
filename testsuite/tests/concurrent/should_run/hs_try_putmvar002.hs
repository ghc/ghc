{-# LANGUAGE MagicHash #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Foreign hiding (void)
import Foreign.C
import GHC.Conc
import GHC.Prim
import System.Environment

-- Measure raw throughput, for M threads that each do N calls to C
-- that call back to hs_try_putmvar() or the foreign export equivalent

main = do
   args <- getArgs
   case args of
     ["1",n,m] -> experiment2 (read m) (experiment1 (read n))
     ["2",n,m] -> experiment2 (read m) (experiment1FE (read n))

-- -----------------------------------------------------------------------------

experiment1 :: Int -> IO ()
experiment1 n = mask_ $ do
  mvar <- newEmptyMVar
  (cap,_) <- threadCapability =<< myThreadId
  replicateM_ n $ do
    sp <- newStablePtrPrimMVar mvar
    externalPutMVar sp cap
    takeMVar mvar

foreign import ccall "externalPutMVar"
  externalPutMVar :: StablePtr PrimMVar
                  -> Int
                  -> IO ()

experiment1FE :: Int -> IO ()
experiment1FE n = do
  mvar <- newEmptyMVar
  (cap,_) <- threadCapability =<< myThreadId
  bracket (newStablePtr mvar) freeStablePtr $ \sp -> do
    replicateM_ n $ do externalPutMVarFE sp cap; takeMVar mvar

foreign import ccall "externalPutMVarFE"
  externalPutMVarFE :: StablePtr (MVar ())
                    -> Int
                   -> IO ()

callbackPutMVar :: StablePtr (MVar ()) -> IO ()
callbackPutMVar sp = do
  mvar <- deRefStablePtr sp
  void $ tryPutMVar mvar ()

foreign export ccall callbackPutMVar :: StablePtr (MVar ()) -> IO ()

-- -----------------------------------------------------------------------------
-- Perform M copies of experiment1 concurrently

experiment2 :: Int -> IO () -> IO ()
experiment2 m exp = do
  mvars <- replicateM m $ do
    m <- newEmptyMVar
    forkFinally exp (\_ -> putMVar m ())
    return m
  mapM_ takeMVar mvars
