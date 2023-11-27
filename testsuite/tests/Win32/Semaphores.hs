module Main where

import Control.Concurrent
  ( forkIO, threadDelay )
import Control.Monad
  ( void )
import Data.Foldable
  ( for_ )

import System.Win32.Event
  ( waitForSingleObject )
import System.Win32.File
  ( closeHandle )
import System.Win32.Semaphore
  ( Semaphore(..), createSemaphore, releaseSemaphore )

main :: IO ()
main = do

  (test_sem, ex1) <- mk_test_sem
  (_, ex2) <- mk_test_sem

  let sem_name = "win32-test-semaphore"
  (sem, ex3) <- createSemaphore Nothing 2 3 (Just sem_name)

  putStrLn (show ex1 ++ " " ++ show ex2 ++ " " ++ show ex3)
  -- False True False

  putStrLn "=========="
  for_ [1,2,3] (run_thread sem)
  -- finish: 1, 2

  putStrLn "=========="
  void $ releaseSemaphore sem 3
  -- finish: 3

  threadDelay 5000   -- 5 ms
  for_ [4,5,6,7] (run_thread sem)
  -- finish: 4, 5

  threadDelay 1000   -- 1 ms
  putStrLn "=========="
  void $ releaseSemaphore sem 1
  -- finish: 6

  threadDelay 100000 -- 100 ms
  putStrLn "=========="
  closeHandle (semaphoreHandle test_sem)
  closeHandle (semaphoreHandle sem)

run_thread :: Semaphore -> Int -> IO ()
run_thread sem i = do
  threadDelay 1000 -- 1 ms
  putStrLn ("start " ++ show i)
  void $ forkIO $ do
    res <- waitForSingleObject (semaphoreHandle sem) 50 -- 50 ms
    putStrLn ("finish " ++ show i ++ ": " ++ show res)

mk_test_sem :: IO (Semaphore, Bool)
mk_test_sem = createSemaphore Nothing 1 1 (Just "test-sem")
