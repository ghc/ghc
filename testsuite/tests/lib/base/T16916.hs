module Main where

import Control.Concurrent
import Foreign.C
import GHC.Clock
import GHC.Event
import System.CPUTime
import System.Posix.Types

foreign import ccall unsafe "socket" c_socket ::
               CInt -> CInt -> CInt -> IO CInt

makeTestSocketFd :: IO Fd
makeTestSocketFd = do
    sockNum <-
        c_socket
            1 -- PF_LOCAL
            2 -- SOCK_DGRAM
            0
    return $ (fromIntegral sockNum :: Fd)

callback :: FdKey -> Event -> IO ()
callback _ _ = return ()

-- Idle CPU usage with 0 for 0% and 10^12 for 100%
idleCpuUsage :: IO Double
idleCpuUsage = do
  startTime <- getMonotonicTime
  startCPUTime <- getCPUTime
  threadDelay 5000000
  endCPUTime <- getCPUTime
  endTime <- getMonotonicTime
  return $ fromIntegral (endCPUTime - startCPUTime) / (endTime - startTime)

main :: IO ()
main = do
  (Just eventMgr) <- getSystemEventManager
  fd <- makeTestSocketFd

  noEventUsage <- idleCpuUsage

  registerFd eventMgr callback fd evtRead OneShot
  registerFd eventMgr callback fd evtWrite OneShot

  eventTriggeredUsage <- idleCpuUsage

  -- CPU consumption should roughly be the same when just idling vs
  -- when idling after the event been triggered
  print $ eventTriggeredUsage / noEventUsage < 10.0
