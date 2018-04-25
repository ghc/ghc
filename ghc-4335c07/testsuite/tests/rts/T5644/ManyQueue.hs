{-# LANGUAGE BangPatterns #-}

module ManyQueue where 

import Control.Concurrent
import Control.Monad

import Conf

newtype MQueue a = MQueue [MVar a]

newMQueue size = do
  lst <- replicateM size newEmptyMVar
  return (MQueue (cycle lst))

writeMQueue :: (MQueue a) -> a -> IO (MQueue a)
writeMQueue (MQueue (x:xs)) el = do
  putMVar x el
  return (MQueue xs)

readMQueue :: (MQueue a) -> IO (MQueue a, a)
readMQueue (MQueue (x:xs)) = do
  el <- takeMVar x
  return ((MQueue xs), el)

testManyQueue'1P1C = do
  print "Test.ManyQueue.testManyQueue'1P1C"
  finished <- newEmptyMVar

  mq <- newMQueue bufferSize
  
  let 
--      elements = [0] ++ [1 .. iTERATIONS] -- workaround
      elements = [0 .. iTERATIONS] -- heap overflow
      
      writer _ 0 = putMVar finished ()
      writer q x = do
                  q' <- writeMQueue q x
                  writer q' (x-1)

      writer' _ [] = putMVar finished ()
      writer' q (x:xs) = do
                  q' <- writeMQueue q x
                  writer' q' xs

      reader _ !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  --forkIO $ writer mq iTERATIONS
  forkIO $ writer' mq elements
  forkIO $ reader mq 0 iTERATIONS

  takeMVar finished
  takeMVar finished

testManyQueue'1P3C = do
  print "Test.ManyQueue.testManyQueue'1P3C"
  let tCount = 3
  finished <- newEmptyMVar

  mqs <- replicateM tCount (newMQueue bufferSize)
  
  let elements = [0 .. iTERATIONS]
      
      writer _ [] = putMVar finished ()
      writer qs (x:xs) = do
                  qs' <- mapM (\q -> writeMQueue q x) qs
                  writer qs' xs

      reader _ !acc 0 = print acc >> putMVar finished ()
      reader q !acc n = do
                  (q', x) <- readMQueue q
                  reader q' (acc+x) (n-1)
  
  forkIO $ writer mqs elements
  mapM_ (\ mq -> forkIO $ reader mq 0 iTERATIONS) mqs

  replicateM (tCount+1) (takeMVar finished)

  return ()