{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Test where

import Control.Concurrent
import Control.Monad
import Foreign.C

-- See also conc059_c.c
--
-- This test fires off some threads that will return after the RTS has
-- shut down.  This should not crash or confuse the RTS.

f :: Int -> IO ()
f x = do
  print x
  replicateM_ 10 $ forkIO $ do millisleep (fromIntegral x); putStrLn "hello"
  return ()

foreign export ccall "f" f :: Int -> IO ()

foreign import ccall safe "millisleep" millisleep :: CInt -> IO ()
