-- !!! RW files 

module Main(main) where

import IO
import Directory ( removeFile, doesFileExist )
import Monad
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  f <- doesFileExist "readwrite001.inout" 
  when f (removeFile "readwrite001.inout")
  hdl <- openFile "readwrite001.inout" ReadWriteMode
#ifdef i386_unknown_mingw32
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

