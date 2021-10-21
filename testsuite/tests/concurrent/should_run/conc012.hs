module Main where

import Control.Concurrent
import Control.Exception
--import GlaExts

data Result = Died SomeExceptionWithLocation | Finished

-- Test stack overflow catching.  Should print "Died: stack overflow".

stackoverflow :: Int -> Int
stackoverflow 0 = 1
stackoverflow n = n + stackoverflow n

main = do
  let x = stackoverflow 1
  result <- newEmptyMVar
  forkIO $ Control.Exception.catch (evaluate x >> putMVar result Finished) $
                     \e -> putMVar result (Died e)
  res <- takeMVar result
  case res of
        Died e -> putStr ("Died: " ++ show e ++ "\n")
        Finished -> putStr "Ok.\n"
