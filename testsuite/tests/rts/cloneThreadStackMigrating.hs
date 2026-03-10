module Main where

import Control.Concurrent
import Control.Monad
import GHC.Exts.Stack
import GHC.Stack.CloneStack

numWorkers :: Int
numWorkers = 100

startN :: Int
startN = 10

runForMicros :: Int
runForMicros = 1000000

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

workerThread :: Int -> IO ()
workerThread n = do
  fib n `seq` pure ()
  workerThread (n + 1)

cloneThread :: ThreadId -> IO ()
cloneThread tid = forever $ do
  snapshot <- cloneThreadStack tid
  stack <- decodeStack snapshot
  stack `seq` pure ()

main :: IO ()
main = do
  tids <- replicateM numWorkers (forkIO $ workerThread startN)
  mapM_ (forkIO . cloneThread) tids
  threadDelay runForMicros
