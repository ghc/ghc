module Main (main) where

-- Test for #10545

import System.Environment
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Random
import System.Mem
import qualified Data.Set as Set

main = do
  [n] <- getArgs
  forkIO $ doSomeWork
  forM [1..read n] $ \n -> do print n; threadDelay 1000; performMinorGC

doSomeWork :: IO ()
doSomeWork = forever $ do
  ns <- replicateM 10000 randomIO :: IO [Int]
  ms <- replicateM 1000 randomIO
  let set = Set.fromList ns
      elems = filter (`Set.member` set) ms
  evaluate $ sum elems
