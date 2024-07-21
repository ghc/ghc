module Main where

import System.IO.Unsafe
import Control.Monad

main :: IO ()
main = do
  foo "test" 10

foo :: String -> Int -> IO ()
foo x n = go n
  where
    oops = unsafePerformIO (putStrLn "Once" >> pure (cycle x))

    go 0 = return ()
    go n = do
      -- `oops` should be shared between loop iterations
      let p  = take n oops
      let !_ = unsafePerformIO (putStrLn p >> pure ())
      go (n-1)
