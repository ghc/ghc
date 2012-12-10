{-# LANGUAGE CPP #-}
import Control.Concurrent.QSem as OldQ

import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay, killThread, yield)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

new = newQSem
wait = waitQSem
signal = signalQSem

--------
-- dummy test-framework

type Assertion = IO ()

x @?= y = when (x /= y) $ error (show x ++ " /= " ++ show y)

testCase :: String -> IO () -> IO ()
testCase n io = putStrLn ("test " ++ n) >> io

defaultMain = sequence
------

main = defaultMain tests

tests = [
    testCase "sem1" sem1,
    testCase "sem2" sem2,
    testCase "sem_kill" sem_kill,
    testCase "sem_fifo" sem_fifo,
    testCase "sem_bracket" sem_bracket
 ]

sem1 :: Assertion
sem1 = do
  q <- new 0
  signal q
  wait q

sem2 :: Assertion
sem2 = do
  q <- new 0
  signal q
  signal q
  wait q
  wait q

sem_fifo :: Assertion
sem_fifo = do
  c <- newChan
  q <- new 0
  t1 <- forkIO $ do wait q; writeChan c 'a'
  threadDelay 10000
  t2 <- forkIO $ do wait q; writeChan c 'b'
  threadDelay 10000
  t3 <- forkIO $ do wait q; writeChan c 'c'
  threadDelay 10000
  signal q
  a <- readChan c
  signal q
  b <- readChan c
  signal q
  c <- readChan c
  [a,b,c] @?= "abc"

sem_kill :: Assertion
sem_kill  = do
  q <- new 0
  t <- forkIO $ do wait q
  threadDelay 100000
  killThread t
  m <- newEmptyMVar
  t <- forkIO $ do wait q; putMVar m ()
  signal q
  takeMVar m


sem_bracket :: Assertion
sem_bracket = do
  q <- new 1
  ts <- forM [1..100000] $ \n -> do
     forkIO $ do bracket_ (wait q) (signal q) (return ())
  mapM_ killThread ts
  wait q

