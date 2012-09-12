{-# LANGUAGE CPP #-}
module Main where

import Control.Exception
import Control.Concurrent
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif

-- the BlockOnDeadMVar exception doesn't cause any output by default

main = do
  forkIO (do m <- newEmptyMVar; takeMVar m)
  print (sum [1..10000])
