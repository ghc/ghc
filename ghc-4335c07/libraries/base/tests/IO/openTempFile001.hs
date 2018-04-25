module Main where

import System.IO
import Control.Exception
import System.Directory

main = bracket 
          (openTempFile "." "test.txt")
          (\(f,_) -> removeFile f)
          (\(f,h) -> do hPutStrLn h $ "\xa9" -- Copyright symbol
                        hClose h
                        s <- readFile f
                        if (s /= "\xa9\n") then error ("failed: " ++ s) else return ())
