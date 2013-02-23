{-# LANGUAGE CPP #-}
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List

new = newQSemN
wait = waitQSemN
signal = signalQSemN

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
    testCase "semn" semn,
    testCase "semn2" semn2,
    testCase "semn3" semn3,
    testCase "semn_kill" semn_kill,
    testCase "semn_bracket" sem_bracket
 ]

semn :: Assertion
semn = do
  c <- newEmptyMVar
  q <- new 0
  t1 <- forkIO $ do wait q 1;  putMVar c 'a'
  threadDelay 10000
  t2 <- forkIO $ do wait q 2;  putMVar c 'b'
  threadDelay 10000
  t3 <- forkIO $ do wait q 3;  putMVar c 'c'
  threadDelay 10000
  signal q 1
  a <-  takeMVar c
  signal q 2
  b <-  takeMVar c
  signal q 3
  c <-  takeMVar c
  [a,b,c] @?= "abc"

semn2 :: Assertion
semn2 = do
  c <- newEmptyMVar
  q <- new 0
  t1 <- forkIO $ do wait q 1; putMVar c 'a'
  threadDelay 10000
  t2 <- forkIO $ do wait q 2; putMVar c 'b'
  threadDelay 10000
  t3 <- forkIO $ do wait q 3; putMVar c 'c'
  threadDelay 10000
  signal q 6
  a <-  takeMVar c
  b <-  takeMVar c
  c <-  takeMVar c
  sort [a,b,c] @?= "abc"

semn3 :: Assertion
semn3 = do
  c <- newEmptyMVar
  q <- new 0
  t1 <- forkIO $ do wait q 1; putMVar c 'a'
  threadDelay 10000
  t2 <- forkIO $ do wait q 2; putMVar c 'b'
  threadDelay 10000
  t3 <- forkIO $ do wait q 3; putMVar c 'c'
  threadDelay 10000
  signal q 3
  a <-  takeMVar c
  b <-  takeMVar c
  threadDelay 10000
  sort [a,b] @?= "ab"
  d <-  isEmptyMVar c
  d @?= True
  signal q 1
  threadDelay 10000
  d <-  isEmptyMVar c
  d @?= True
  signal q 2
  x <-  takeMVar c
  x @?= 'c'

semn_kill :: Assertion
semn_kill  = do
  q <- new 0
  t <- forkIO $ do wait q 1
  threadDelay 10000
  killThread t
  m <- newEmptyMVar
  t <- forkIO $ do wait q 1; putMVar m ()
  signal q 1
  takeMVar m

sem_bracket :: Assertion
sem_bracket = do
  q <- new 1
  ts <- forM [1..100000] $ \n -> do
     forkIO $ do bracket_ (wait q 1) (signal q 1) (return ())
  mapM_ killThread ts
  wait q 1
