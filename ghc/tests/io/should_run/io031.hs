--!!! RW files 
module Main(main) where

import IO
import Directory ( removeFile )

main = do 
  hdl <- openFile "io031.inout" ReadWriteMode
  removeFile "io031.inout"
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

