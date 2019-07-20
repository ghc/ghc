module Main where

import Control.Concurrent
import Foreign.C
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

idleCpuUsage :: IO Integer
idleCpuUsage = do
  startCPUTime <- getCPUTime
  threadDelay 500000
  endCPUTime <- getCPUTime
  return $ endCPUTime - startCPUTime

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
  print $ (fromIntegral eventTriggeredUsage / fromIntegral noEventUsage) < 2.0
