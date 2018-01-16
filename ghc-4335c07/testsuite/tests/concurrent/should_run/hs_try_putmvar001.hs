{-# LANGUAGE MagicHash #-}
module Main where

import Control.Concurrent
import Control.Exception
import Foreign
import Foreign.C
import GHC.Conc
import GHC.Prim

-- Sample code demonstrating proper use of hs_try_putmvar()

main = do
   makeExternalCall >>= print
   threadDelay 100000

makeExternalCall :: IO CInt
makeExternalCall = mask_ $ do
  mvar <- newEmptyMVar
  sp <- newStablePtrPrimMVar mvar -- freed by hs_try_takemvar()
  fp <- mallocForeignPtr
  withForeignPtr fp $ \presult -> do
    (cap,_) <- threadCapability =<< myThreadId
    scheduleCallback sp cap presult
    takeMVar mvar `onException` forkIO (do takeMVar mvar; touchForeignPtr fp)
      -- the C callback will still run if takeMVar is interrupted, so the
      -- exception handler keeps the result memory alive long enough.
    peek presult

foreign import ccall "scheduleCallback"
  scheduleCallback :: StablePtr PrimMVar
                   -> Int
                   -> Ptr CInt
                   -> IO ()
