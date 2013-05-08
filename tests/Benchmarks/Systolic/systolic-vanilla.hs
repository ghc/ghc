-------------------------------------------------------------------------------
--- $Id: Bench1.hs#4 2005/06/14 01:10:17 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where
import System.Time
import System.Random
import System.IO
import System.Environment
import Control.Monad
import Control.Concurrent

systolicFilter :: [Double] -> [Double] -> [Double]
systolicFilter weights inputStream
  = [sum [a*x | (a,x) <- zip weights xs]
              | xs <- staggerBy (length weights) inputStream]

staggerBy n list | length list <= n = []
staggerBy n list
  = take n list : staggerBy n (tail list)

applyFilter rgen resultMV
  = do let weights = take 10 (randomRs (0.0, 10.0) rgen)
       let inputStream = take 2000 (randomRs (0.0, 100.0) rgen)
       let result = last (systolicFilter weights inputStream)
       putMVar resultMV result

rgens 0 _ = []
rgens n rgen
  = nextGen : rgens (n-1) nextGen
    where
    (_, nextGen) = split rgen

main
  = do instances <- getArgs >>= readIO . head
       putStrLn "SMP Systolic Filter Benchmarks"
       dones <- sequence (replicate instances newEmptyMVar)
       rgen1 <- getStdGen
       let gens = rgens instances rgen1
       t1 <- getClockTime
       sequence [forkIO (applyFilter (gens!!i) (dones!!i)) |
                 i <- [0..instances-1]]
       rs <- sequence [takeMVar (dones!!i) | i <- [0..instances-1]]
       sequence [putStrLn (show (rs!!i)) | i <- [0..instances-1]]
       t2 <- getClockTime
       putStrLn ("Time: " ++ (timeDiffToString (diffClockTimes t2 t1)))
