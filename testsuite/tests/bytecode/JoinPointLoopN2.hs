-- The leg of the safepoint that only the threaded RTS reaches: a collection
-- started by another capability stops ours by clearing HpLim, and the loop
-- yields in place without any heap check of its own having failed.
--
-- main runs LoopShapes.count, which allocates nothing, so nothing inside the
-- loop would ever ask to collect; the forked sibling allocates on the other
-- capability and its collections are what stop the loop. Each of them walks
-- the stack the loop left, resume frame on top, and the loop only gets to
-- finish because it was resumed at the right pc every time.
{-# OPTIONS_GHC -fno-full-laziness #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import LoopShapes (count)

main :: IO ()
main = do
  ref <- newIORef []
  _ <- forkIO (churn ref 0)
  print (count 2000000)

-- | Cons forever into an 'IORef', dropping the list every thousand cells:
-- allocation that no optimisation can remove and that keeps no memory. It
-- prints nothing, so the test's output is the loop's answer alone.
churn :: IORef [Int] -> Int -> IO ()
churn ref k = do
  xs <- readIORef ref
  writeIORef ref (if k `rem` 1000 == 0 then [] else k : xs)
  churn ref (k + 1)
