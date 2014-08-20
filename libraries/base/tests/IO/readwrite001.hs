-- !!! RW files 

module Main(main) where

import System.IO
import System.Directory ( removeFile, doesFileExist )
import Control.Monad

main = do
  f <- doesFileExist "readwrite001.inout" 
  when f (removeFile "readwrite001.inout")
  hdl <- openFile "readwrite001.inout" ReadWriteMode
  hSetBuffering hdl LineBuffering
  hPutStr hdl "as"
  hSeek hdl AbsoluteSeek 0
  ch <- hGetChar hdl
  print ch
  hPutStr hdl "ase"
  hSeek hdl AbsoluteSeek 0
  putChar '\n'
  ls <- hGetContents hdl
  putStrLn ls

