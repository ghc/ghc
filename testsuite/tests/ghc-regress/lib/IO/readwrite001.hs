-- !!! RW files 

module Main(main) where

import IO
import Directory ( removeFile, doesFileExist )
import Monad
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  f <- doesFileExist "readwrite001.inout" 
  when f (removeFile "readwrite001.inout")
  hdl <- openFile "readwrite001.inout" ReadWriteMode
#ifdef mingw32_HOST_OS
  hSetBinaryMode hdl True
#endif
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

