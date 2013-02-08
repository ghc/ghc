{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign
import Control.Concurrent

type Fun = Int -> IO Int

foreign import ccall "wrapper" mkF :: Fun -> IO (FunPtr Fun)

foreign import ccall "dynamic" callF :: FunPtr Fun -> Fun

-- This test should create 4 OS threads only:
--   one for main
--   worker 1 for the IO manager
--   worker 2 to run the first forkIO
--   worker 3 created when worker 2 makes its foreign call

-- Due to #4850, an extra worker was being created because worker 2 was
-- lost after returning from its foreign call.

main = do
  m <- newEmptyMVar
  callback m >> takeMVar m >>= print
  callback m >> takeMVar m >>= print

callback m =
  forkIO $ do
    f <- mkF $ \x -> return (x+1)
    r <- callF f 3
    putMVar m r
