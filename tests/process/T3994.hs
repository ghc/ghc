module Main where

import Control.Concurrent
import System.IO
import System.Process

main :: IO ()
main = do (_,Just hout,_,p) <- createProcess (proc "./T3994app" ["start", "10000"])
                            { std_out = CreatePipe, create_group = True }
          start <- hGetLine hout
          putStrLn start
          interruptProcessGroupOf p
          t <- myThreadId
               -- timeout
          forkIO $ do
            threadDelay 5000000
            putStrLn "Interrupting a Running Process Failed"
            hFlush stdout
            killThread t
          waitForProcess p
          putStrLn "end"
          return ()
