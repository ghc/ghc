-- !!! RW files 
module Main(main) where

import IO
import Directory ( removeFile, doesFileExist )
import Monad

main = do
  f <- doesFileExist "io031.inout" 
  when f (removeFile "io031.inout")
  hdl <- openFile "io031.inout" ReadWriteMode
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

