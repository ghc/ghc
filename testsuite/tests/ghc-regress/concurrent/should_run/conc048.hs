module Main where

import GHC.Conc
import Control.Concurrent
import Control.Exception
import IO
import Foreign.StablePtr
import System.IO

-- Create two tvars each holding 0
initTVars :: STM (TVar Int, TVar Int)
initTVars = do v1 <- newTVar 0
	       v2 <- newTVar 0
               return (v1, v2)

-- Increment v1, retry
optionOne :: TVar Int -> TVar Int -> STM ()
optionOne v1 v2 = do x <- readTVar v1
                     writeTVar v1 (x + 10)
                     retry

-- Increment v2, don't retry
optionTwo :: TVar Int -> TVar Int -> STM ()
optionTwo v1 v2 = do x <- readTVar v2
                     writeTVar v2 (x + 10)

-- Combine options one and two.  We should be left with optionTwo because
-- optionOne attempts to retry while valid.
elseTestA :: TVar Int -> TVar Int -> STM ()
elseTestA v1 v2 = (optionOne v1 v2) `orElse` (optionTwo v1 v2)

-- Combine options one and two.  We should be left with optionTwo because
-- optionOne attempts to retry while valid.
elseTestB :: TVar Int -> TVar Int -> STM ()
elseTestB v1 v2 = (optionTwo v1 v2) `orElse` (optionOne v1 v2)

-- Combine options two and one.  We should be left with optionTwo because
-- it completes successfully.
elseTestC :: TVar Int -> TVar Int -> STM ()
elseTestC v1 v2 = (optionTwo v1 v2) `orElse` (optionTwo v1 v2)

-- Nested use of `orElse`: combine (optionOne and OptionOne) with optionTwo
elseTestD :: TVar Int -> TVar Int -> STM ()
elseTestD v1 v2 = ((optionOne v1 v2) `orElse` (optionOne v1 v2)) `orElse` (optionTwo v1 v2)

-- Nested use of `orElse`: combine (optionOne and optionTwo) with optionTwo
elseTestE :: TVar Int -> TVar Int -> STM ()
elseTestE v1 v2 = ((optionOne v1 v2) `orElse` (optionTwo v1 v2)) `orElse` (optionTwo v1 v2)

-- Combine options one and one.  Retry should propagate.
elseTestZ :: TVar Int -> TVar Int -> STM ()
elseTestZ v1 v2 = (optionOne v1 v2) `orElse` (optionOne v1 v2)

-- return (v1, v2)
snapshot :: TVar Int -> TVar Int -> STM (Int, Int)
snapshot v1 v2 = do s1 <- readTVar v1
                    s2 <- readTVar v2
                    return (s1, s2)

main :: IO ()
main = do newStablePtr stdout
          iteration 10

iteration :: Int -> IO ()
iteration n = 
       do (sv1, sv2) <- atomically ( initTVars )

          atomically ( elseTestA sv1 sv2 )
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          atomically ( elseTestB sv1 sv2 )
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          atomically ( elseTestC sv1 sv2 )
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          atomically ( elseTestD sv1 sv2 )
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          atomically ( elseTestE sv1 sv2 )
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          Control.Exception.catch (atomically ( elseTestZ sv1 sv2 ))
                 (\e -> putStr ("Caught: " ++ (show e) ++ "\n"))
          vs <- atomically ( snapshot sv1 sv2 )
          print vs

          if (n == 0) then return () else iteration (n - 1)



