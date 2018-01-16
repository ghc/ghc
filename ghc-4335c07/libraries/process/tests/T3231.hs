module Main (main) where

import Control.Concurrent
import System.IO
import System.Cmd
import System.Directory

main = do
    hSetBuffering stdout NoBuffering
    forkIO $ f "foo1.txt"
    forkIO $ f "foo2.txt"
    threadDelay $ 2*1000000
    putStrLn "Finished successfully"

f file = do
    h <- openFile file WriteMode
    hPutStrLn h "fjkladsf"
    system "sleep 1"
    -- putChar '.'
    hClose h
    removeFile file
    f file
