{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Foreign.C
import GHC.Clock
import GHC.Event
import System.CPUTime
import System.Posix.Types
import Control.Monad
import System.Exit

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
idleCpuUsage :: IO Integer
idleCpuUsage = do
  -- measure the process time spent in the rts, not in the mutator
  -- make sure to disable idle GC (+RTS -I0)
  !startCPUTime <- getCPUTime
  GHC.Event.threadDelay 100000
  !endCPUTime <- getCPUTime
  let !t = endCPUTime - startCPUTime
  return $ t

main :: IO ()
main = do
  (Just eventMgr) <- getSystemEventManager
  fd <- makeTestSocketFd

  let getAvgCpuUsage = do
        let n = 10
        let warmup = 2
        xs <- drop warmup <$> replicateM (warmup+n) idleCpuUsage
        return $! fromIntegral (sum xs) / fromIntegral n

  !before <- getAvgCpuUsage

  registerFd eventMgr callback fd evtRead OneShot
  registerFd eventMgr callback fd evtWrite OneShot

  -- use this to test that this test works
  --forkIO $ forever $ do
  --  putStrLn ""
  --  threadDelay 10000

  !after <- getAvgCpuUsage

  -- CPU consumption should roughly be the same when just idling vs
  -- when idling after the event has been triggered
  let r = (after-before) / before * 100

  let max_percent = 100 -- max difference (in percent)

  when (abs r > max_percent) $ do
    putStrLn $ mconcat
      [ "Idle CPU consumption too different after event registration: "
      , if r > 0 then "+" else ""
      , show (round r)
      , "% (> +/- "
      , show (round max_percent)
      , "%)\n"
      , "Before: "
      , show (round before `div` 1000000 :: Integer)
      , "ms\n"
      , "After: "
      , show (round after `div` 1000000 :: Integer)
      , "ms"
      ]
    exitFailure
