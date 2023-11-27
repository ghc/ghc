module Main where

import Control.Concurrent
import System.IO
import System.Process

launch :: String -> IO String
launch i = do (hin,hout,herr,ph) <- runInteractiveProcess "cat" [] Nothing Nothing
              -- forkIO $ collect ph  -- This doesn't seem to be relevant to the problem.
              forkIO $ do hPutStr hin i
                          hClose hin
              hGetContents hout

main :: IO ()
main = do o <- foldl (>>=) (return "foo") (replicate 5 launch)
          t <- myThreadId
               -- timeout
          forkIO $ do threadDelay 5000000; killThread t
          putStrLn o
