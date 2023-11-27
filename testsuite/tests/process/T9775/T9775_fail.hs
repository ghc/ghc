module Main where

import System.Process

main
 = do (_,_,_,p) <- createProcess (proc "main" [])
      waitForProcess p >>= print
