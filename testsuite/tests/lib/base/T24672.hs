module Main where

import Control.Concurrent
import qualified System.Posix.Process as SPP
import System.IO

main = do
  print "before SPP.forkProcess"
  hFlush stdout
  threadDelay (2*1000*1000)
  SPP.forkProcess $ pure ()
  threadDelay (2*1000*1000)
  print "after SPP.forkProcess"
