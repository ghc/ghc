module Main where

import Control.Concurrent
import System.IO
import System.Process

main :: IO ()
main = do p <- foldl (>>=) (return stdin) (replicate 10 docat) >>= docat0
          t <- myThreadId
               -- timeout
          forkIO $ do threadDelay 5000000; killThread t
          waitForProcess p
          putStrLn "end"
          return ()

docat :: Handle -> IO Handle
docat hin = do
  (_, Just hout, _, ph) <- 
         createProcess (proc "cat" []){ std_in = UseHandle hin,
                                        std_out = CreatePipe }
  return hout

docat0 :: Handle -> IO ProcessHandle
docat0 hin = do
  (_,_,_,ph) <-  createProcess (proc "cat" []){ std_in = UseHandle hin }
  return ph
