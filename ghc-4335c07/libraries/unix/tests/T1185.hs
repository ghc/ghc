module Main where

import Control.Concurrent
import System.Posix
import System.IO
import System.Exit

main =
    do putStrLn "running..."
       (stdinr, stdinw) <- createPipe
       (stdoutr, stdoutw) <- createPipe
       pid <- forkProcess $ do hw <- fdToHandle stdoutw
                               hr <- fdToHandle stdinr
                               closeFd stdinw
                               hGetContents hr >>= hPutStr hw
                               hClose hr
                               hClose hw
                               exitImmediately ExitSuccess
       threadDelay 100000
       closeFd stdoutw
       closeFd stdinw
       hr2 <- fdToHandle stdoutr
       hGetContents hr2 >>= putStr
       getProcessStatus True False pid >>= print
